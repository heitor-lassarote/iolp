module Language.LowCode.Logic.Analyzer
    ( AnalyzerState (..)
    , LogicAnalyzerT
    , emptyState
    , evalAnalyzerT
    , execAnalyzerT
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
    deriving (Eq, Show)

data Warning
    = UnusedVariable Text
    deriving (Eq, Show)

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

type LogicAnalyzerT = State AnalyzerState

emptyState :: AnalyzerState
emptyState = AnalyzerState Map.empty [] []

evalAnalyzerT :: AnalyzerState -> LogicAnalyzerT a -> a
evalAnalyzerT st = runIdentity . flip evalStateT st

execAnalyzerT :: AnalyzerState -> LogicAnalyzerT a -> AnalyzerState
execAnalyzerT st = runIdentity . flip execStateT st

withScope :: LogicAnalyzerT a -> LogicAnalyzerT a
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

addError :: Error -> LogicAnalyzerT ()
addError e = modify \st -> st { errors = e : errors st }

--addWarning :: Warning -> LogicAnalyzerT ()
--addWarning w = modify \st -> st { warnings = w : warnings st }

--tryMapVariable :: (VariableInfo -> VariableInfo) -> Text -> LogicAnalyzerT ()
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

--variableCheck :: Text -> LogicAnalyzerT ()
--variableCheck = tryMapVariable id

--markUsed :: Text -> LogicAnalyzerT ()
--markUsed = tryMapVariable (\info -> info { unused = False })

analyzeAssign :: Text -> Expression -> LogicAnalyzerT ()
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

analyzeVar :: Text -> VariableType -> Expression -> LogicAnalyzerT ()
analyzeVar var type' expr = do
    symbols <- gets analyzerSymbols
    if Map.member var symbols
    then addError $ ShadowedVariable var
    else do
        analyzeExprTypes var type' expr
        let info = mkInfo var type'
        modify \st -> st { analyzerSymbols = Map.insert var info symbols }

checkTypes :: Text -> VariableType -> VariableType -> LogicAnalyzerT ()
checkTypes name expectedType actualType
    | expectedType == actualType = pure ()
    | otherwise = addError $ TypeMismatch name expectedType actualType

analyzeExprTypes :: Text -> VariableType -> Expression -> LogicAnalyzerT ()
analyzeExprTypes exprName expectedType expr =
    whenJustM (analyzeExpr expr) (checkTypes exprName expectedType)

-- In case Call has a name instead of expr:
--checkApply :: Name -> VariableType -> [Expression] -> LogicAnalyzerT (Maybe VariableType)
--checkApply name (FunctionType argTypes ret) arguments = do
--    let nArgs = length arguments
--        nTypes = length argTypes
--    when (nArgs /= nTypes) $
--        addError $ IncompatibleSignatures name nArgs nTypes
--    traverse_ (uncurry (analyzeExprTypes name)) $ zip argTypes arguments
--    pure $ Just ret
--checkApply name _ _ = addError (NotAFunction name) *> pure Nothing

-- TODO: Generate more descriptive names!
checkApply :: VariableType -> [Expression] -> LogicAnalyzerT (Maybe VariableType)
checkApply (FunctionType argTypes ret) arguments = do
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

    mkSymbol (Start _ name ret@(FunctionType _ _) _ _) acc =
        Map.insert name (mkInfo name ret) acc
    mkSymbol _ acc = acc

analyzeMany :: Environment -> [AST] -> LogicAnalyzerT ()
analyzeMany env asts = do
    let functions = collectSymbols env asts
    modify \st -> st { analyzerSymbols = functions }
    traverse_ analyzeStart asts

analyze :: Environment -> AST -> LogicAnalyzerT ()
analyze env ast = analyzeMany env [ast]
{-# INLINE analyze #-}

analyzeStart :: AST -> LogicAnalyzerT ()
analyzeStart (Start _ name (FunctionType argTypes ret) arguments next) = do
    let nArgs = length arguments
        nTypes = length argTypes
    when (nArgs /= nTypes) $ addError $ IncompatibleSignatures name nArgs nTypes
    let argsInfo = Map.fromList $ zipWith (\n t -> (n, mkInfo n t)) arguments argTypes
    modify \st ->
        st { analyzerSymbols = Map.union argsInfo (analyzerSymbols st) }
    whenJustM (withScope $ analyzeImpl next) $ checkTypes name ret
analyzeStart (Start _ name _ _ next) =
    addError (NotAFunction name) *> void (withScope $ analyzeImpl next)
analyzeStart next =
    addError StartIsNotFirstSymbol *> void (withScope $ analyzeImpl next)

analyzeImpl :: AST -> LogicAnalyzerT (Maybe VariableType)
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

analyzeExpr :: Expression -> LogicAnalyzerT (Maybe VariableType)
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
        typeE <- analyzeExpr expr
        case typeE of
            Nothing -> pure Nothing
            Just typeF -> checkApply typeF arguments
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

analyzeArray :: [Expression] -> LogicAnalyzerT (Maybe VariableType)
analyzeArray [] = pure $ Just UnitType  -- TODO: What's the correct type of an empty array?
analyzeArray (x : xs) = do
    xTypeMaybe <- analyzeExpr x
    xsTypesMaybe <- traverse analyzeExpr xs
    let yTypeMaybe = join $ find (/= xTypeMaybe) xsTypesMaybe
    case (xTypeMaybe, yTypeMaybe) of
        (Just xType, Just yType) -> addError (TypeMismatch "array" xType yType) *> pure xTypeMaybe
        (Just _    , Nothing   ) -> pure xTypeMaybe  -- Everything is ok.
        (Nothing   , Just _    ) -> pure yTypeMaybe  -- More than one thing failed.
        (Nothing   , Nothing   ) -> pure xTypeMaybe  -- First failed.

vtType :: Variable -> LogicAnalyzerT (Maybe VariableType)
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
