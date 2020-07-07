module Language.LowCode.Logic.Analyzer
    ( Error (..)
    , prettyError
    , Warning (..)
    , prettyWarning
    , AnalyzerState (..)
    , LogicAnalyzer
    , emptyState
    , evalAnalyzer
    , execAnalyzer
    , analyzeMany
    , analyze
    , analyzeExpr
    , vtType
    , isNumeric
    , isText
    , interactsWithUnary
    , interactsWithBinary
    ) where

import Universum

import qualified Data.Map.Strict as Map

import           Language.Common
import           Language.LowCode.Logic.AST

-- TODO: Should IncompatibleTypes and TypeMismatch really have these signatures?
-- Or even better: Should TypeMismatch instead have info about the node instead
-- of simply a Text?
data Error
    = IncompatibleSignatures Name Int Int
    | IncompatibleTypes1 UnarySymbol Expression
    | IncompatibleTypes2 Expression BinarySymbol Expression
    | NotAFunction Name
    | ShadowedVariable Name
    | StartIsNotFirstSymbol
    | TypeMismatch Text VariableType VariableType
    | UndefinedVariable Name
    | UnknownType Name VariableType
    deriving (Eq, Show)

prettyError :: Error -> Text
prettyError = \case
    IncompatibleSignatures name args1 args2 ->
        "'" <> name <> "' expects " <> show args1 <> " arguments, but " <> show args2 <> " were given."
    IncompatibleTypes1 symbol expr ->
        "Incompatible expression '" <> codegenE expr <> "' for unary operator '" <> unaryToText symbol <> "'."
    IncompatibleTypes2 left symbol right ->
        "Incompatible expressions '" <> codegenE left <> "' and '" <> codegenE right <> "' for unary operator '" <> binaryToText symbol <> "'."
    NotAFunction name ->
        -- TODO: Add types/expressions being applied? And maybe the inferred type?
        "Can't use '" <> name <> "' as function."
    ShadowedVariable name ->
        -- TODO: Add where it was defined and what is the new value being assigned. Maybe old value too.
        "'" <> name <> "' was shadowed."
    StartIsNotFirstSymbol ->
        "The Start symbol must appear iff it's the first symbol."
    TypeMismatch name expected actual ->
        "Type mismatch in '" <> name <> "'. Expected '" <> show expected <> "' but got '" <> show actual <> "'."
    UndefinedVariable name ->
        -- TODO: Scan for similarly named variables.
        "'" <> name <> "' was used but it was not defined. Perhaps you forgot to declare it or made a typo?"
    UnknownType location actual ->
        "Could not deduce type '" <> show actual <> "' from its use in '" <> location <> "'."

data Warning
    = UnusedVariable Name
    deriving (Eq, Show)

prettyWarning :: Warning -> Text
prettyWarning = \case
    UnusedVariable name ->
        "Declared but not used: '" <> name <> "'."

data VariableInfo = VariableInfo
    { varName :: Name
    , unused  :: Bool
    , varType :: VariableType
    } deriving (Show)

mkInfo :: Name -> VariableType -> VariableInfo
mkInfo name type' = VariableInfo name True type'

data AnalyzerState = AnalyzerState
    { analyzerSymbols :: Map Text VariableInfo
    , errors          :: [Error]
    , warnings        :: [Warning]
    } deriving (Show)

type LogicAnalyzer = State AnalyzerState

emptyState :: AnalyzerState
emptyState = AnalyzerState Map.empty [] []

evalAnalyzer :: AnalyzerState -> LogicAnalyzer a -> a
evalAnalyzer st = runIdentity . flip evalStateT st

execAnalyzer :: AnalyzerState -> LogicAnalyzer a -> AnalyzerState
execAnalyzer st = runIdentity . flip execStateT st

withScope :: LogicAnalyzer a -> LogicAnalyzer a
withScope action = do
    symbols <- gets analyzerSymbols
    ret <- action
    newSymbols <- flip Map.difference symbols <$> gets analyzerSymbols
    warnings' <- gets warnings
    modify \st -> st
        { analyzerSymbols = symbols
        , warnings = Map.foldrWithKey appendUnused warnings' newSymbols
        }
    pure ret
  where
    appendUnused var info acc =
        if unused info then UnusedVariable var : acc else acc

addError :: Error -> LogicAnalyzer ()
addError e = modify \st -> st { errors = e : errors st }

--addWarning :: Warning -> LogicAnalyzer ()
--addWarning w = modify \st -> st { warnings = w : warnings st }

--tryMapVariable :: (VariableInfo -> VariableInfo) -> Text -> LogicAnalyzer ()
--tryMapVariable f var = do
--    symbols <- gets analyzerSymbols
--    newSymbols <- Map.alterF
--        (\case
--            Nothing -> do
--                addError $ UndefinedVariable var
--                pure Nothing
--            Just val ->
--                pure $ Just $ f val)
--        var
--        symbols
--    modify \st -> st { analyzerSymbols = newSymbols }

--variableCheck :: Text -> LogicAnalyzer ()
--variableCheck = tryMapVariable id

--markUsed :: Text -> LogicAnalyzer ()
--markUsed = tryMapVariable (\info -> info { unused = False })

analyzeAssign :: Text -> Expression -> LogicAnalyzer ()
analyzeAssign var expr = do
    symbols <- gets analyzerSymbols
    case Map.lookup var symbols of
        Nothing -> addError $ UndefinedVariable var
        Just info -> whenJustM (analyzeExpr expr) \exprType ->
            if varType info == exprType
            then let info' = info { varType = exprType }
                 in
                 modify \st ->
                    st { analyzerSymbols = Map.insert var info' symbols }
            else addError $ TypeMismatch var (varType info) exprType

analyzeVar :: Text -> VariableType -> Expression -> LogicAnalyzer ()
analyzeVar var type' expr = do
    symbols <- gets analyzerSymbols
    if Map.member var symbols
    then addError $ ShadowedVariable var
    else do
        analyzeExprTypes var type' expr
        let info = mkInfo var type'
        modify \st -> st { analyzerSymbols = Map.insert var info (analyzerSymbols st) }

checkTypes :: Text -> VariableType -> VariableType -> LogicAnalyzer ()
checkTypes name expectedType actualType
    | expectedType == actualType = pure ()
    | otherwise = addError $ TypeMismatch name expectedType actualType

analyzeExprTypes :: Text -> VariableType -> Expression -> LogicAnalyzer ()
analyzeExprTypes exprName (ArrayType innerTypeA) (Value (Constant (Array xs))) =
    traverse_ (analyzeExprTypes exprName innerTypeA) xs
analyzeExprTypes exprName expectedType (Value (Constant (Array _))) =
    addError $ UnknownType exprName expectedType
analyzeExprTypes exprName expectedType expr =
    whenJustM (analyzeExpr expr) (checkTypes exprName expectedType)

-- TODO: Generate more descriptive names!
checkApply :: [Expression] -> VariableType -> LogicAnalyzer (Maybe VariableType)
checkApply arguments (FunctionType argTypes ret) = do
    let nArgs = length arguments
        nTypes = length argTypes
    when (nArgs /= nTypes) $
        addError $ IncompatibleSignatures "function" nArgs nTypes
    traverse_ (uncurry (analyzeExprTypes "function")) $ zip argTypes arguments
    pure $ Just ret
checkApply _ _ = addError (NotAFunction "not function lol") *> pure Nothing

collectSymbols :: Environment -> [AST] -> Map Name VariableInfo
collectSymbols env = foldr mkSymbol externsInfo
  where
    externsInfo = Map.mapWithKey mkInfo (externs env)

    -- TODO: Should we collect all symbols?
    mkSymbol (Start _ name ret@(FunctionType _ _) _ _) acc =
        Map.insert name (mkInfo name ret) acc
    mkSymbol _ acc = acc

analyzeMany :: Environment -> [AST] -> LogicAnalyzer ()
analyzeMany env asts = do
    let functions = collectSymbols env asts
    modify \st -> st { analyzerSymbols = functions }
    traverse_ (withScope . analyzeStart) asts

analyze :: Environment -> AST -> LogicAnalyzer ()
analyze env ast = analyzeMany env [ast]
{-# INLINE analyze #-}

analyzeStart :: AST -> LogicAnalyzer ()
analyzeStart (Start _ name (FunctionType argTypes ret) arguments next) = do
    let nArgs = length arguments
        nTypes = length argTypes
    when (nArgs /= nTypes) $ addError $ IncompatibleSignatures name nArgs nTypes
    let argsInfo = Map.fromList $ zipWith (\n t -> (n, mkInfo n t)) arguments argTypes
    modify \st ->
        st { analyzerSymbols = Map.union argsInfo (analyzerSymbols st) }
    whenJustM (analyzeImpl next) $ checkTypes name ret
analyzeStart (Start _ name _ _ next) =
    addError (NotAFunction name) *> void (analyzeImpl next)
analyzeStart next =
    addError StartIsNotFirstSymbol *> void (analyzeImpl next)

analyzeImpl :: AST -> LogicAnalyzer (Maybe VariableType)
analyzeImpl = \case
    Assign _ var expr next -> do
        analyzeAssign var expr
        analyzeImpl next
    End -> pure $ Just UnitType
    Expression _ expr next -> do
        void $ analyzeExpr expr
        analyzeImpl next
    If _ expr false true next -> do
        analyzeExprTypes "if" BoolType expr
        void $ withScope $ analyzeImpl false
        void $ withScope $ analyzeImpl true
        analyzeImpl next
    Return _ expr -> maybe (pure $ Just UnitType) analyzeExpr expr
    Start _ _ _ _ next -> addError StartIsNotFirstSymbol *> analyzeImpl next
    Var _ var type' expr next -> do
        analyzeVar var type' expr
        analyzeImpl next
    While _ expr body next -> do
        analyzeExprTypes "while" BoolType expr
        void $ withScope $ analyzeImpl body
        analyzeImpl next

analyzeExpr :: Expression -> LogicAnalyzer (Maybe VariableType)
analyzeExpr = \case
    BinaryOp left symbol' right -> do
        typeL <- analyzeExpr left
        typeR <- analyzeExpr right
        let ret = do typeL' <- typeL
                     typeR' <- typeR
                     interactsWithBinary typeL' symbol' typeR'
        maybe (addError (IncompatibleTypes2 left symbol' right) *> pure Nothing)
              (pure . Just)
              ret
    Call expr arguments -> do
        -- In case Call has a name instead of expr:
        --(infoMaybe, symbols) <- gets $
        --    Map.updateLookupWithKey markUsed name . analyzerSymbols
        --modify \st -> st { analyzerSymbols = symbols }
        --case infoMaybe of
        --    Nothing -> addError (UndefinedVariable name) *> pure Nothing
        --    Just (VariableInfo _ _ typeF) -> checkApply name typeF arguments
        typeFMaybe <- analyzeExpr expr
        case typeFMaybe of
            Nothing -> pure Nothing
            Just typeF -> do
                checkApply arguments typeF
    Parenthesis expr -> analyzeExpr expr
    UnaryOp symbol' expr -> do
        typeE <- analyzeExpr expr
        let ret = interactsWithUnary symbol' =<< typeE
        maybe (addError (IncompatibleTypes1 symbol' expr) *> pure Nothing)
              (pure . Just)
              ret
    Value value -> case value of
        Variable v -> analyzeVariable v
        Constant c -> vtType c
  where
    markUsed _ info = Just info { unused = False }

    analyzeVariable name = do
        (infoMaybe, symbols) <- gets $
            Map.updateLookupWithKey markUsed name . analyzerSymbols
        modify \st -> st { analyzerSymbols = symbols }
        maybe (addError (UndefinedVariable name) *> pure Nothing)
              (pure . Just . varType)
              infoMaybe

analyzeArray :: [Expression] -> LogicAnalyzer (Maybe VariableType)
analyzeArray [] = pure Nothing
analyzeArray (x : xs) = do
    xTypeMaybe <- analyzeExpr x
    xsTypesMaybe <- traverse analyzeExpr xs
    let yTypeMaybe = join $ find (/= xTypeMaybe) xsTypesMaybe
    case (xTypeMaybe, yTypeMaybe) of
        (Just xType, Just yType) -> addError (TypeMismatch "array" xType yType) *> pure xTypeMaybe
        (Just _    , Nothing   ) -> pure xTypeMaybe  -- Everything is ok.
        (Nothing   , Just _    ) -> pure yTypeMaybe  -- More than one thing failed.
        (Nothing   , Nothing   ) -> pure xTypeMaybe  -- First failed.

vtType :: Variable -> LogicAnalyzer (Maybe VariableType)
vtType = \case
    Array es  -> ArrayType <<$>> analyzeArray es
    Bool _    -> pure $ Just BoolType
    Char _    -> pure $ Just CharType
    Double _  -> pure $ Just DoubleType
    Integer _ -> pure $ Just IntegerType
    Text _    -> pure $ Just TextType
    Unit      -> pure $ Just UnitType

isNumeric :: VariableType -> Bool
isNumeric = \case
    DoubleType  -> True
    IntegerType -> True
    _           -> False

isText :: VariableType -> Bool
isText = \case
    CharType -> True
    TextType -> True
    _        -> False

interactsWithUnary :: UnarySymbol -> VariableType -> Maybe VariableType
interactsWithUnary Negate = \case
    DoubleType  -> Just DoubleType
    IntegerType -> Just IntegerType
    _           -> Nothing
interactsWithUnary Not = \case
    BoolType    -> Just BoolType
    IntegerType -> Just IntegerType
    _           -> Nothing

interactsWithBinary :: VariableType -> BinarySymbol -> VariableType -> Maybe VariableType
interactsWithBinary TextType Add TextType = Just TextType  -- Concatenate strings.
interactsWithBinary BoolType s BoolType
    | isLogical s = Just BoolType  -- Logical operators only interact with logical variables.
    | otherwise   = Nothing
interactsWithBinary l s r
    | l /= r                        = Nothing  -- Different types never interact.
    | isArithmetic s && isNumeric l = Just l  -- Numeric types interact with all arithmetic operators.
    | isComparison s                = Just BoolType  -- Comparisons always return booleans.
    | otherwise                     = Nothing
