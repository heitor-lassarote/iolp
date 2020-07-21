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
    , analyzeExprWithHint
    , analyzeType
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
        ("Incompatible expressions '" % stext % "' and '" % stext % "' for binary operator '" % stext % "'.")
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
            typeE <- analyzeExprWithHint (varType info) expr
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
        checkTypes var type' =<< analyzeExprWithHint type' expr
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
    typeE <- analyzeExprWithHint ret expr
    when (ret /= typeE) $ addError $ TypeMismatch (codegenE expr) ret typeE

analyzeImpl :: AST -> Analyzer ()
analyzeImpl = \case
    Assign _ var expr next -> do
        analyzeAssign var expr
        analyzeImpl next
    End -> pure ()
    Expression _ expr next -> do
        void $ analyzeExpr expr
        analyzeImpl next
    If _ expr false true next -> do
        checkTypes "if" BoolType =<< analyzeExprWithHint BoolType expr
        void $ withScope $ analyzeImpl false
        void $ withScope $ analyzeImpl true
        analyzeImpl next
    Return _ expr -> analyzeReturn expr
    Start _ _ _ _ next -> addError StartIsNotFirstSymbol *> analyzeImpl next
    Var _ var type' expr next -> do
        analyzeVar var type' expr
        analyzeImpl next
    While _ expr body next -> do
        checkTypes "while" BoolType =<< analyzeExprWithHint BoolType expr
        void $ withScope $ analyzeImpl body
        analyzeImpl next

analyzeExpr :: Expression -> Analyzer (Maybe VariableType)
analyzeExpr = \case
    Access left right -> do
        typeLMaybe <- analyzeExpr left
        case typeLMaybe of
            Nothing -> pure Nothing
            Just typeL@(RecordType fields) -> do
                let fieldSearch = find (\l -> fst l == right) fields
                whenNothing_ fieldSearch $ addError (NotAMember typeL right)
                pure $ Just typeL
            Just typeL -> addError (NotARecord typeL right) *> pure (Just typeL)
    BinaryOp left symbol' right -> do
        typeLMaybe <- analyzeExpr left
        case typeLMaybe of
            Nothing -> pure Nothing
            Just typeL -> do
                typeR <- analyzeExprWithHint typeL right
                let ret = interactsWithBinary typeL symbol' typeR
                whenNothing_ ret $
                    addError (IncompatibleTypes2 left symbol' right)
                pure ret
    Call (Value (Variable name)) arguments -> do
        infoMaybe <- analyzeVariable name
        case infoMaybe of
            Nothing -> addError (UndefinedVariable name) *> pure Nothing
            Just (VariableInfo _ typeF@(FunctionType argTypes _)) -> do
                typeAs <- traverse (uncurry analyzeExprWithHint) $ zip argTypes arguments
                Just <$> analyzeApply name typeAs typeF
            Just (VariableInfo _ _) -> do
                addError (NotAFunction name) *> pure Nothing
    Call expr arguments -> do
        let exprC = codegenE expr
        typeEMaybe <- analyzeExpr expr
        case typeEMaybe of
            Nothing -> pure Nothing
            Just (typeF@(FunctionType argTypes _)) -> do
                typeAs <- traverse (uncurry analyzeExprWithHint) $ zip argTypes arguments
                Just <$> analyzeApply exprC typeAs typeF
            Just type' -> addError (NotAFunction exprC) *> pure (Just type')
    Index left right -> do
        typeLMaybe <- analyzeExpr left
        void $ analyzeExprWithHint IntegerType right
        case typeLMaybe of
            Nothing -> pure ()
            Just (ArrayType _) -> pure ()
            Just typeL ->
                addError (TypeMismatch (codegenE left) (ArrayType typeL) typeL)
        pure typeLMaybe
    Parenthesis expr -> analyzeExpr expr
    UnaryOp symbol' expr -> do
        typeEMaybe <- analyzeExpr expr
        let ret = interactsWithUnary symbol' =<< typeEMaybe
        whenNothing_ ret $
            addError (IncompatibleTypes1 symbol' expr)
        pure ret
    Value value -> case value of
        Variable v -> varType <<$>> analyzeVariable v
        Constant c -> analyzeType c

analyzeExprWithHint :: VariableType -> Expression -> Analyzer VariableType
analyzeExprWithHint !expectedType = \case
    Access left right -> do
        typeLMaybe <- analyzeExpr left
        case typeLMaybe of
            Nothing -> pure expectedType
            Just typeL@(RecordType fields) -> do
                let fieldSearch = find (\l -> fst l == right) fields
                case fieldSearch of
                    Nothing -> do
                        addError (NotAMember typeL right)
                        pure expectedType
                    Just field -> pure $ snd field
            Just typeL -> addError (NotARecord typeL right) *> pure typeL
    BinaryOp left symbol' right -> do
        typeLMaybe <- analyzeExpr left
        case typeLMaybe of
            Nothing -> pure expectedType
            Just typeL -> do
                typeR <- analyzeExprWithHint typeL right
                let ret = interactsWithBinary typeL symbol' typeR
                maybe
                    (addError (IncompatibleTypes2 left symbol' right) *> pure expectedType)
                    pure
                    ret
    Call (Value (Variable name)) arguments -> do
        infoMaybe <- analyzeVariable name
        case infoMaybe of
            Nothing -> addError (UndefinedVariable name) *> pure expectedType
            Just (VariableInfo _ typeF@(FunctionType argTypes _)) -> do
                typeAs <- traverse (uncurry analyzeExprWithHint) $ zip argTypes arguments
                analyzeApply name typeAs typeF
            Just (VariableInfo _ type') -> do
                addError (NotAFunction name)
                pure type'
    Call expr arguments -> do
        let exprC = codegenE expr
        typeEMaybe <- analyzeExpr expr
        case typeEMaybe of
            Nothing -> pure expectedType
            Just (typeF@(FunctionType argTypes _)) -> do
                typeAs <- traverse (uncurry analyzeExprWithHint) $ zip argTypes arguments
                analyzeApply exprC typeAs typeF
            Just typeE -> addError (NotAFunction exprC) *> pure typeE
    Index left right -> do
        let leftC  = codegenE left
            rightC = codegenE right
        typeL <- analyzeExprWithHint (ArrayType expectedType) left
        typeR <- analyzeExprWithHint IntegerType right
        typeL' <- case typeL of
            ArrayType actualType
                | actualType == expectedType -> pure actualType
                | otherwise                  -> do
                    addError (TypeMismatch leftC (ArrayType expectedType) typeL)
                    pure expectedType
            _ -> do
                addError (TypeMismatch leftC (ArrayType expectedType) typeL)
                pure expectedType
        case typeR of
            IntegerType -> pure ()
            _           -> addError (TypeMismatch rightC IntegerType typeR)
        pure typeL'
    Parenthesis expr -> analyzeExprWithHint expectedType expr
    UnaryOp symbol' expr -> do
        typeE <- analyzeExprWithHint expectedType expr
        let ret = interactsWithUnary symbol' typeE
        maybe
            (addError (IncompatibleTypes1 symbol' expr) *> pure expectedType)
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

analyzeArray :: [Expression] -> Analyzer (Maybe VariableType)
analyzeArray [] = pure Nothing
analyzeArray (x : xs) = do
    xTypeMaybe <- analyzeExpr x
    case xTypeMaybe of
        Nothing    -> pure Nothing
        Just xType -> do
            --xsTypes <- traverse (analyzeExprWithHint xType) xs
            --let yTypeMaybe = find (/= xType) xsTypes
            --whenJust yTypeMaybe \yType ->
            --    addError (TypeMismatch (codegenE x) xType yType)
            traverse_ (analyzeExprWithHint xType) xs
            pure xTypeMaybe

-- TODO: Stil not properly typechecking!
analyzeArrayWithHint :: VariableType -> [Expression] -> Analyzer VariableType
analyzeArrayWithHint expectedType [] = pure $ ArrayType $ unnestArray expectedType
analyzeArrayWithHint expectedType (x : xs) = do
    let expectedType' = unnestArray expectedType
    xType <- analyzeExprWithHint expectedType' x
    traverse_ (analyzeExprWithHint expectedType') xs
    when (xType == expectedType) $
        addError $ TypeMismatch (codegenE x) (ArrayType expectedType) xType
    pure $ ArrayType xType

unnestArray :: VariableType -> VariableType
unnestArray (ArrayType typeA) = typeA
unnestArray type'             = type'

analyzeRecord :: Name -> [(Name, Expression)] -> Analyzer (Maybe VariableType)
analyzeRecord name fields = do
    -- Find whether there are fields with duplicate names:
    let (names, exprs) = unzip fields
        sorted = sort names
        dups = filter (uncurry (==)) $ zip sorted (drop 1 sorted)
    traverse_ (errorDuplicate . fst) dups

    recs <- asks records
    case Map.lookup name recs of
        Nothing -> do
            addError $ NoSuchRecord name
            typesMaybe <- sequence <$> traverse analyzeExpr exprs
            case typesMaybe of
                Nothing -> pure Nothing
                Just types -> pure $ Just $ RecordType $ zip names types
        Just templateFields -> do
            -- Check if number of fields match.
            let (templateNames, templateTypes) = unzip templateFields
                templateSorted = sort templateNames
            when (templateSorted /= sorted) $
                addError $ IncompatibleRecord name templateSorted sorted
            traverse_ (uncurry analyzeExprWithHint) $ zip templateTypes exprs
            traverse_ (uncurry analyzeWithHint) $ zip templateTypes exprs
            pure $ Just $ RecordType templateFields
  where
    -- TODO: Add types to error?
    errorDuplicate = addError . DuplicateRecord name

    analyzeWithHint expectedType' (Value (Constant var)) =
        analyzeTypeWithHint expectedType' var
    analyzeWithHint expectedType' _ = pure expectedType'

analyzeRecordWithHint :: VariableType -> Name -> [(Name, Expression)] -> Analyzer VariableType
analyzeRecordWithHint expectedType name fields = do
    typeRMaybe <- analyzeRecord name fields
    maybe (pure expectedType) pure typeRMaybe

analyzeType :: Variable -> Analyzer (Maybe VariableType)
analyzeType = \case
    Array es    -> analyzeArray es
    Bool _      -> pure $ Just BoolType
    Char _      -> pure $ Just CharType
    Double _    -> pure $ Just DoubleType
    Integer _   -> pure $ Just IntegerType
    Record r fs -> analyzeRecord r fs
    Text _      -> pure $ Just TextType
    Unit        -> pure $ Just UnitType

analyzeTypeWithHint :: VariableType -> Variable -> Analyzer VariableType
analyzeTypeWithHint expectedType = \case
    Array es    -> analyzeArrayWithHint expectedType es
    Record r fs -> analyzeRecordWithHint expectedType r fs
    var         -> do
        typeVMaybe <- analyzeType var
        case typeVMaybe of
            Nothing -> pure expectedType  -- Absurd.
            Just typeV
                | typeV == expectedType -> pure typeV
                | otherwise             -> do
                    addError (TypeMismatch (codegenV var) expectedType typeV)
                    pure typeV

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
