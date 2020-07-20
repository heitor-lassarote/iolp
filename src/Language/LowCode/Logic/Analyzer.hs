module Language.LowCode.Logic.Analyzer
    ( Error (..)
    , prettyError
    , Warning (..)
    , prettyWarning
    , AnalyzerReader (..)
    , AnalyzerWriter (..)
    , AnalyzerState (..)
    , Analyzer
    , runAnalyzer
    , runAnalyzer'
    , evalAnalyzer
    , evalAnalyzer'
    , execAnalyzer
    , execAnalyzer'
    , analyzeMany
    , analyze
    , analyzeExpr
    , analyzeTypeWithHint
    , isNumeric
    , isText
    , interactsWithUnary
    , interactsWithBinary
    ) where

import           Universum hiding (asks, gets, modify)

import           Control.Monad.Trans.RWS.CPS
import           Data.Default.Class
import qualified Data.Map.Strict as Map
import           Formatting (int, sformat, shown, stext, (%))

import           Language.Common
import           Language.LowCode.Logic.AST

-- TODO: Should IncompatibleTypes and TypeMismatch really have these signatures?
-- Or even better: Should TypeMismatch instead have info about the node instead
-- of simply a Text?
data Error
    = DuplicateRecord Name Name
    | IncompatibleRecord Name [Name] [Name]
    | IncompatibleSignatures Name Int Int
    | IncompatibleTypes1 UnarySymbol Expression
    | IncompatibleTypes2 Expression BinarySymbol Expression
    | NoSuchRecord Name
    | NotAFunction Name
    | NotAMember VariableType Name
    | NotARecord VariableType Name
    | ShadowedVariable Name
    | StartIsNotFirstSymbol
    | TypeMismatch Text VariableType VariableType
    | UndefinedVariable Name
    | UnknownType Name VariableType
    deriving (Eq, Show)

prettyError :: Error -> Text
prettyError = \case
    DuplicateRecord recordName fieldName -> sformat
        ("Duplicate field '" % stext % "' on record '" % stext % "'.")
        fieldName
        recordName
    IncompatibleRecord name expectedFields actualFields -> sformat
        ("'" % stext % "' expects the following fields: " % shown % ", but " % shown % " were given.")
        name
        expectedFields
        actualFields
    IncompatibleSignatures name args1 args2 -> sformat
        ("'" % stext % "' expects " % int % " arguments, but " % int % " were given.")
        name
        args1
        args2
    IncompatibleTypes1 symbol expr -> sformat
        ("Incompatible expression '" % stext % "' for unary operator '" % stext % "'.")
        (codegenE expr)
        (unaryToText symbol)
    IncompatibleTypes2 left symbol right -> sformat
        ("Incompatible expressions '" % stext % "' and '" % stext % "' for unary operator '" % stext % "'.")
        (codegenE left)
        (codegenE right)
        (binaryToText symbol)
    NoSuchRecord name -> sformat
        ("Could not find record '" % stext % "'.")
        name
    NotAFunction name -> sformat
        -- TODO: Add types/expressions being applied? And maybe the inferred type?
        ("Can't use '" % stext % "' as function.")
        name
    NotAMember record memberName -> sformat
        ("'" % stext % "' is not a member of the record '" % shown % "'.")
        memberName
        record
    NotARecord type' memberName -> sformat
        ("Can't access '" % stext % "' on non-record type '" % shown % "'.")
        memberName
        type'
    ShadowedVariable name -> sformat
        -- TODO: Add where it was defined and what is the new value being assigned. Maybe old value too.
        ("'" % stext % "' was shadowed.")
        name
    StartIsNotFirstSymbol ->
        "The Start symbol must appear iff it's the first symbol."
    TypeMismatch name expected actual -> sformat
        ("Type mismatch in '" % stext % "'. Expected '" % shown % "' but got '" % shown % "'.")
        name
        expected
        actual
    UndefinedVariable name -> sformat
        -- TODO: Scan for similarly named variables.
        ("'" % stext % "' was used but it was not defined. Perhaps you forgot to declare it or made a typo?")
        name
    UnknownType location actual -> sformat
        ("Could not deduce type '" % shown % "' from its use in '" % stext % "'.")
        actual
        location

data Warning
    = UnusedVariable Name
    deriving (Eq, Show)

prettyWarning :: Warning -> Text
prettyWarning = \case
    UnusedVariable name -> sformat
        ("Declared but not used: '" % stext % "'.")
        name

data VariableInfo = VariableInfo
    { unused  :: !Bool
    , varType :: !VariableType
    } deriving (Show)

mkInfo :: VariableType -> VariableInfo
mkInfo = VariableInfo True

data AnalyzerReader = AnalyzerReader
    { records    :: Map Name [(Name, VariableType)]
    , returnType :: VariableType
    } deriving (Show)

instance Default AnalyzerReader where
    def = AnalyzerReader Map.empty UnitType

data AnalyzerWriter = AnalyzerWriter
    { errors          :: ![Error]
    , warnings        :: ![Warning]
    } deriving (Show)

instance Semigroup AnalyzerWriter where
    AnalyzerWriter e w <> AnalyzerWriter e' w' = AnalyzerWriter (e <> e') (w <> w')

instance Monoid AnalyzerWriter where
    mempty = AnalyzerWriter [] []

instance Default AnalyzerWriter where
    def = mempty

data AnalyzerState = AnalyzerState
    { analyzerSymbols :: Map Text VariableInfo
    } deriving (Show)

instance Default AnalyzerState where
    def = AnalyzerState Map.empty

type Analyzer = RWS AnalyzerReader AnalyzerWriter AnalyzerState

runAnalyzer
    :: Analyzer a
    -> AnalyzerReader
    -> AnalyzerState
    -> (a, AnalyzerState, AnalyzerWriter)
runAnalyzer = runRWS

runAnalyzer' :: Analyzer a -> (a, AnalyzerState, AnalyzerWriter)
runAnalyzer' a = runAnalyzer a def def

evalAnalyzer
    :: Analyzer a
    -> AnalyzerReader
    -> AnalyzerState
    -> (a, AnalyzerWriter)
evalAnalyzer = evalRWS

evalAnalyzer' :: Analyzer a -> (a, AnalyzerWriter)
evalAnalyzer' a = evalAnalyzer a def def

execAnalyzer
    :: Analyzer a
    -> AnalyzerReader
    -> AnalyzerState
    -> (AnalyzerState, AnalyzerWriter)
execAnalyzer = execRWS

execAnalyzer' :: Analyzer a -> (AnalyzerState, AnalyzerWriter)
execAnalyzer' a = execAnalyzer a def def

withScope :: Analyzer a -> Analyzer a
withScope action = do
    symbols <- gets analyzerSymbols
    ret <- action
    newSymbols <- flip Map.difference symbols <$> gets analyzerSymbols
    modify \s -> s { analyzerSymbols = symbols }
    writer (ret, mempty { warnings = Map.foldrWithKey appendUnused [] newSymbols })
  where
    appendUnused var info acc =
        if unused info then UnusedVariable var : acc else acc

addError :: Error -> Analyzer ()
addError e = tell $ mempty { errors = [e] }

--addWarning :: Warning -> Analyzer ()
--addWarning w = tell $ mempty { warnings = [w] }

analyzeAssign :: Text -> Expression -> Analyzer ()
analyzeAssign var expr = do
    symbols <- gets analyzerSymbols
    case Map.lookup var symbols of
        Nothing -> addError $ UndefinedVariable var
        Just info -> do
            typeE <- analyzeExpr (varType info) expr
            if varType info == typeE
                then
                    let info' = info { varType = typeE }
                    in modify \s -> s { analyzerSymbols = Map.insert var info' symbols }
                else addError $ TypeMismatch var (varType info) typeE

analyzeVar :: Text -> VariableType -> Expression -> Analyzer ()
analyzeVar var type' expr = do
    symbols <- gets analyzerSymbols
    if Map.member var symbols
    then addError $ ShadowedVariable var
    else do
        checkTypes var type' =<< analyzeExpr type' expr
        let info = mkInfo type'
        modify \s -> s { analyzerSymbols = Map.insert var info (analyzerSymbols s) }

checkTypes :: Text -> VariableType -> VariableType -> Analyzer ()
checkTypes name expectedType actualType
    | expectedType == actualType = pure ()
    | otherwise = addError $ TypeMismatch name expectedType actualType

collectStarts :: Environment -> [AST] -> Map Name VariableInfo
collectStarts env = foldr mkSymbol (Map.map mkInfo (externs env))
  where
    mkSymbol (Start _ name ret@(FunctionType _ _) _ _) acc =
        Map.insert name (mkInfo ret) acc
    mkSymbol _ acc = acc

analyzeMany :: Environment -> [AST] -> Analyzer ()
analyzeMany env asts = do
    let functions = collectStarts env asts
    withRWS (\r s ->
        ( r { records = recordTemplates env }
        , s { analyzerSymbols = functions }
        ))
        (traverse_ (withScope . analyzeStart) asts)

analyze :: Environment -> AST -> Analyzer ()
analyze env ast = analyzeMany env [ast]
{-# INLINE analyze #-}

analyzeStart :: AST -> Analyzer ()
analyzeStart (Start _ name (FunctionType argTypes ret) arguments next) = do
    let nArgs = length arguments
        nTypes = length argTypes
    when (nArgs /= nTypes) $ addError $ IncompatibleSignatures name nArgs nTypes
    let argsInfo = Map.fromList $ zipWith (\n t -> (n, mkInfo t)) arguments argTypes
    withRWS (\r s ->
        ( r { returnType = ret }
        , s { analyzerSymbols = Map.union argsInfo (analyzerSymbols s) }
        ))
        (analyzeImpl next)
analyzeStart (Start _ name _ _ next) =
    addError (NotAFunction name) *> void (analyzeImpl next)
analyzeStart next =
    addError StartIsNotFirstSymbol *> void (analyzeImpl next)

analyzeReturn :: Maybe Expression -> Analyzer ()
analyzeReturn Nothing = do
    ret <- asks returnType
    when (ret /= UnitType) $ addError $ TypeMismatch "return" ret UnitType
analyzeReturn (Just expr) = do
    ret <- asks returnType
    typeE <- analyzeExpr ret expr
    when (ret /= typeE) $ addError $ TypeMismatch "return" ret typeE

analyzeImpl :: AST -> Analyzer ()
analyzeImpl = \case
    Assign _ var expr next -> do
        analyzeAssign var expr
        analyzeImpl next
    End -> pure ()
    Expression _ expr next -> do
        -- TODO: What is the correct type in this case? One solution is take the
        -- C# route and disallow arbitrary expressions, but rather make it become
        -- a function call.
        void $ analyzeExpr UnitType expr
        analyzeImpl next
    If _ expr false true next -> do
        checkTypes "if" BoolType =<< analyzeExpr BoolType expr
        void $ withScope $ analyzeImpl false
        void $ withScope $ analyzeImpl true
        analyzeImpl next
    Return _ expr -> analyzeReturn expr
    Start _ _ _ _ next -> addError StartIsNotFirstSymbol *> analyzeImpl next
    Var _ var type' expr next -> do
        analyzeVar var type' expr
        analyzeImpl next
    While _ expr body next -> do
        checkTypes "while" BoolType =<< analyzeExpr BoolType expr
        void $ withScope $ analyzeImpl body
        analyzeImpl next

analyzeExpr :: VariableType -> Expression -> Analyzer VariableType
analyzeExpr !expectedType = \case
    Access left right -> do
        -- TODO: Maybe add a withField?
        -- TODO: Port Call code to Access.
        analyzeExpr expectedType (Call (Value (Variable right)) [left])
    BinaryOp left symbol' right -> mdo
        -- FIXME: This freezes due to the recursive bindings!
        typeL <- analyzeExpr typeR left
        typeR <- analyzeExpr typeL right
        let ret = interactsWithBinary typeL symbol' typeR
        maybe (addError (IncompatibleTypes2 left symbol' right) *> pure expectedType)
              pure
              ret
    Call (Value (Variable name)) arguments -> do
        (infoMaybe, symbols) <- gets $
            Map.updateLookupWithKey (const (Just . markUsed)) name . analyzerSymbols
        modify \s -> s { analyzerSymbols = symbols }
        case infoMaybe of
            Nothing -> addError (UndefinedVariable name) *> pure expectedType
            Just (VariableInfo _ typeF@(FunctionType argTypes _)) -> do
                typeAs <- traverse (uncurry analyzeExpr) $ zip argTypes arguments
                analyzeApply name typeAs typeF
            Just (VariableInfo _ _) -> do
                addError (NotAFunction name) *> pure expectedType
    Call expr arguments -> mdo
        let exprC = codegenE expr
            expectedFunctionType = FunctionType typeAs expectedType
        -- TODO: Check whether typeE will freeze with typeAs, since they are
        -- recursive.
        typeE <- analyzeExpr expectedFunctionType expr
        (typeAs, ret) <- case typeE of
            typeF@(FunctionType argTypes _) -> do
                typeAs' <- traverse (uncurry analyzeExpr) $ zip argTypes arguments
                ret' <- analyzeApply exprC typeAs' typeF
                pure (typeAs', ret')
            _ -> addError (NotAFunction exprC) *> pure ([], expectedType)
        pure ret
    Index left right -> do
        typeL <- analyzeExpr (ArrayType expectedType) left
        typeR <- analyzeExpr IntegerType right
        case typeL of
            ArrayType actualType
                | actualType == expectedType -> pure ()
                | otherwise -> addError (TypeMismatch (codegenE left) (ArrayType expectedType) typeL)
            _ -> addError (TypeMismatch (codegenE left) (ArrayType expectedType) typeL)
        case typeR of
            IntegerType -> pure ()
            _ -> addError (TypeMismatch (codegenE right) IntegerType typeR)
        pure expectedType
    Parenthesis expr -> analyzeExpr expectedType expr
    UnaryOp symbol' expr -> do
        typeE <- analyzeExpr expectedType expr
        let ret = interactsWithUnary symbol' typeE
        maybe (addError (IncompatibleTypes1 symbol' expr) *> pure expectedType)
              pure
              ret
    Value value -> case value of
        Variable v -> maybe expectedType varType <$> analyzeVariable v
        Constant c -> analyzeTypeWithHint expectedType c

analyzeApply :: Name -> [VariableType] -> VariableType -> Analyzer VariableType
analyzeApply name arguments (FunctionType argTypes ret) = do
    let nArgs = length arguments
        nTypes = length argTypes
    when (nArgs /= nTypes) $ addError $ IncompatibleSignatures name nArgs nTypes
    traverse_ (uncurry (checkTypes name)) $ zip argTypes arguments
    pure ret
analyzeApply name _ type' = addError (NotAFunction name) *> pure type'

markUsed :: VariableInfo -> VariableInfo
markUsed info = info { unused = False }

analyzeVariable :: Name -> Analyzer (Maybe VariableInfo)
analyzeVariable name = do
    (infoMaybe, symbols) <- gets $
        Map.updateLookupWithKey (const (Just . markUsed)) name . analyzerSymbols
    modify \s -> s { analyzerSymbols = symbols }
    whenNothing_ infoMaybe $ addError (UndefinedVariable name)
    pure infoMaybe

analyzeArray :: VariableType -> [Expression] -> Analyzer VariableType
analyzeArray expectedType exprs = do
    types <- traverse (unnestArray expectedType) exprs
    case types of
        [] -> pure expectedType
        (x : _)
            | x == expectedType -> pure x
            | otherwise         -> do
                addError $ TypeMismatch "array" expectedType x
                pure x
  where
    unnestArray (ArrayType typeA) (Value (Constant (Array xs))) = do
        traverse_ (unnestArray typeA) xs
        pure typeA
    unnestArray type' expr = do
        typeE <- analyzeExpr type' expr
        when (type' /= typeE) $
            addError $ TypeMismatch (codegenE expr) type' typeE
        pure typeE

    --nestArray 0 typeA = typeA
    --nestArray n typeA = ArrayType (nestArray (n - 1) typeA)

    --arrayDepth (ArrayType typeA) = 1 + arrayDepth typeA
    --arrayDepth _                 = 0

analyzeRecord :: VariableType -> Name -> [(Name, Expression)] -> Analyzer VariableType
analyzeRecord expectedType name fields = do
    -- Find whether there are fields with duplicate names:
    let (names, exprs) = unzip fields
        sorted = sort names
        dups = filter (uncurry (==)) $ zip sorted (drop 1 sorted)
    traverse_ (errorDuplicate . fst) dups

    recs <- asks records
    case Map.lookup name recs of
        Nothing -> do
            --traverse_ (analyzeExpr Nothing) exprs
            addError $ NoSuchRecord name
            pure expectedType
        Just templateFields -> do
            -- Check if number of fields match.
            let (templateNames, templateTypes) = unzip templateFields
                templateSorted = sort templateNames
            when (templateSorted /= sorted) $
                addError $ IncompatibleRecord name templateSorted sorted
            traverse_ (uncurry analyzeExpr) $ zip templateTypes exprs
            traverse_ (uncurry analyzeWithHint) $ zip templateTypes exprs
            pure expectedType
  where
    -- TODO: Add types to error?
    errorDuplicate = addError . DuplicateRecord name

    analyzeWithHint expectedType' (Value (Constant var)) =
        analyzeTypeWithHint expectedType' var
    analyzeWithHint expectedType' _ = pure expectedType'

analyzeTypeWithHint :: VariableType -> Variable -> Analyzer VariableType
analyzeTypeWithHint expectedType = \case
    Array es    -> analyzeArray expectedType es
    Bool _      -> pure BoolType
    Char _      -> pure CharType
    Double _    -> pure DoubleType
    Integer _   -> pure IntegerType
    Record r fs -> analyzeRecord expectedType r fs
    Text _      -> pure TextType
    Unit        -> pure UnitType

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
