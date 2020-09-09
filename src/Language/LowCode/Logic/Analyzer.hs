module Language.LowCode.Logic.Analyzer
    ( AnalyzerReader (..)
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
    , isNumeric
    , isText
    , interactsWithUnary
    , interactsWithBinary
    ) where

import           Universum hiding (Type, ask, asks, find, get, gets, modify)

import           Control.Monad.Morph
import           Control.Monad.Trans.RWS
import           Data.Default.Class
import           Data.Foldable (find)
import qualified Data.Map.Strict as Map

import           Language.Codegen (unsafeCodegen')
import           Language.Common
import           Language.LowCode.Logic.AST
import           Language.LowCode.Logic.Error
import           Utility

data VariableInfo = VariableInfo
    { unused  :: !Bool
    , varType :: !Type
    } deriving (Show)

mkInfo :: Type -> VariableInfo
mkInfo = VariableInfo True

data AnalyzerReader = AnalyzerReader
    { environment :: Environment
    , returnType  :: Type
    } deriving (Show)

instance Default AnalyzerReader where
    def = AnalyzerReader def unitType

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

type AnalyzerT = RWST AnalyzerReader AnalyzerWriter AnalyzerState
type Analyzer = AnalyzerT Identity

runAnalyzer
    :: (Monad m)
    => AnalyzerT m a
    -> AnalyzerReader
    -> AnalyzerState
    -> m (a, AnalyzerState, AnalyzerWriter)
runAnalyzer = runRWST

runAnalyzer' :: (Monad m) => AnalyzerT m a -> m (a, AnalyzerState, AnalyzerWriter)
runAnalyzer' a = runAnalyzer a def def

evalAnalyzer
    :: (Monad m)
    => AnalyzerT m a
    -> AnalyzerReader
    -> AnalyzerState
    -> m (a, AnalyzerWriter)
evalAnalyzer = evalRWST

evalAnalyzer' :: (Monad m) => AnalyzerT m a -> m (a, AnalyzerWriter)
evalAnalyzer' a = evalAnalyzer a def def

execAnalyzer
    :: (Monad m)
    => AnalyzerT m a
    -> AnalyzerReader
    -> AnalyzerState
    -> m (AnalyzerState, AnalyzerWriter)
execAnalyzer = execRWST

execAnalyzer' :: (Monad m) => AnalyzerT m a -> m (AnalyzerState, AnalyzerWriter)
execAnalyzer' a = execAnalyzer a def def

--hoistMaybe :: AnalyzerT Maybe a -> Analyzer (Maybe a)
--hoistMaybe ama = fst <<$>> (evalRWST ama <$> ask <*> get)

withScope :: (Monad m) => AnalyzerT m a -> AnalyzerT m a
withScope action = do
    symbols <- gets analyzerSymbols
    ret <- action
    newSymbols <- flip Map.difference symbols <$> gets analyzerSymbols
    modify \s -> s { analyzerSymbols = symbols }
    writer (ret, mempty { warnings = Map.foldrWithKey appendUnused [] newSymbols })
  where
    appendUnused var info acc =
        if unused info then UnusedVariable var : acc else acc

addError :: (Monad m) => Error -> AnalyzerT m ()
addError e = tell $ mempty { errors = [e] }

--addWarning :: (Monad m) => Warning -> AnalyzerT m ()
--addWarning w = tell $ mempty { warnings = [w] }

analyzeAssign
    :: Expression metadata
    -> Expression metadata
    -> AnalyzerT Maybe (Expression Type, Expression Type)
analyzeAssign left right = do
    exprL <- analyzeExpr left
    let typeL = getMetadata exprL
    exprR <- analyzeExprWithHint typeL right
    let typeR = getMetadata exprR
    when (typeL /= typeR) $
        addError $ TypeMismatch (unsafeCodegen' left <> " = " <> unsafeCodegen' right) typeL typeR
    pure (exprL, exprR)

analyzeVar :: Name -> Type -> Expression metadata -> AnalyzerT Maybe (Expression Type)
analyzeVar name type' expr = do
    symbols <- gets analyzerSymbols
    exprR <- analyzeExprWithHint type' expr
    if Map.member name symbols
        then addError (ShadowedVariable name)
        else do
            hoist generalize $ checkTypes name type' (getMetadata exprR)
            let info = mkInfo type'
            modify \s -> s { analyzerSymbols = Map.insert name info (analyzerSymbols s) }
    pure exprR

checkTypes :: Text -> Type -> Type -> Analyzer ()
checkTypes name expectedType actualType
    | expectedType == actualType = pure ()
    | otherwise = addError $ TypeMismatch name expectedType actualType

collectStarts :: Environment -> [TopLevel expressionMetadata metadata] -> Map Name VariableInfo
collectStarts env = foldr mkSymbol (Map.map mkInfo (externs env))
  where
    mkSymbol (Start _ name ret@(FunctionType _ _) _ _) acc =
        Map.insert name (mkInfo ret) acc
    mkSymbol _ acc = acc

analyzeMany :: Environment -> [TopLevel expressionMetadata metadata] -> AnalyzerT Maybe [TopLevel Type metadata]
analyzeMany env asts =
    withRWST (\r s ->
        ( r { environment = env }
        , s { analyzerSymbols = collectStarts env asts }
        ))
        (traverse (withScope . analyzeStart) asts)

analyze :: Environment -> TopLevel expressionMetadata metadata -> AnalyzerT Maybe [TopLevel Type metadata]
analyze env ast = analyzeMany env [ast]
{-# INLINE analyze #-}

analyzeStart :: TopLevel expressionMetadata metadata -> AnalyzerT Maybe (TopLevel Type metadata)
analyzeStart (Start _ name (FunctionType argTypes ret) arguments next) = do
    let nArgs = length arguments
        nTypes = length argTypes
    when (nArgs /= nTypes) $
        addError $ IncompatibleSignatures name nArgs nTypes
    let argsInfo = Map.fromList $ zipWith (\n t -> (n, mkInfo t)) arguments argTypes
    ast <- withRWST (\r s ->
        ( r { returnType = ret }
        , s { analyzerSymbols = Map.union argsInfo (analyzerSymbols s) }
        ))
        (analyzeImpl next)
    pure $ Start (getMetadata ast) name (FunctionType argTypes ret) arguments ast
analyzeStart (Start _ name _ _ next) =
    addError (NotAFunction name) *> analyzeImpl next *> lift Nothing

analyzeReturn :: Maybe (Expression metadata) -> AnalyzerT Maybe (Expression Type)
analyzeReturn Nothing = do
    ret <- asks returnType
    when (ret /= unitType) $
        addError $ TypeMismatch "return" ret unitType
    pure $ Structure unitType unit
analyzeReturn (Just expr) = do
    ret <- asks returnType
    exprE <- analyzeExprWithHint ret expr
    let typeE = getMetadata exprE
    when (ret /= typeE) $
        addError $ TypeMismatch (unsafeCodegen' expr) ret typeE
    pure exprE

analyzeImpl :: AST expressionMetadata metadata -> AnalyzerT Maybe (AST Type metadata)
analyzeImpl = \case
    Assign m left right next -> do
        (left', right') <- analyzeAssign left right
        Assign m left' right' <$> analyzeImpl next
    End m -> pure $ End m
    Expression m expr next -> Expression m <$> analyzeExpr expr <*> analyzeImpl next
    If m cond true false next -> do
        cond' <- analyzeExprWithHint boolType cond
        hoist generalize $ checkTypes "if" boolType (getMetadata cond')
        true'  <- withScope $ analyzeImpl true
        false' <- withScope $ analyzeImpl false
        If m cond' true' false' <$> analyzeImpl next
    Return m exprMaybe -> Return m <$> (Just <$> analyzeReturn exprMaybe)
    Var m var type' expr next -> do
        Var m var type' <$> analyzeVar var type' expr <*> analyzeImpl next
    While m cond body next -> do
        cond' <- analyzeExprWithHint boolType cond
        hoist generalize $ checkTypes "while" boolType (getMetadata cond')
        body' <- withScope $ analyzeImpl body
        While m cond' body' <$> analyzeImpl next

analyzeExpr :: Expression metadata -> AnalyzerT Maybe (Expression Type)
analyzeExpr = \case
    Access _ left right -> do
        exprL <- analyzeExpr left
        let typeL = getMetadata exprL
        case typeL of
            RecordType fieldTypes -> do
                let fieldSearch = find (\(Field l _) -> l == right) fieldTypes
                whenNothing_ fieldSearch $ addError (NotAMember typeL right)
                case fieldSearch of
                    Nothing -> do
                        addError (NotAMember typeL right)
                        pure $ Access typeL exprL right
                    Just (Field _ fieldType) -> pure $ Access fieldType exprL right
            _ -> do
                addError (NotARecord typeL right)
                -- TODO: Should we return Nothing?
                pure $ Access typeL exprL right
    BinaryOp _ left symbol' right -> do
        exprL <- analyzeExpr left
        let typeL = getMetadata exprL
        exprR <- analyzeExprWithHint typeL right
        let typeR = getMetadata exprR
        case interactsWithBinary typeL symbol' typeR of
            Nothing -> do
                addError (IncompatibleTypes2 (void left) symbol' (void right))
                pure $ BinaryOp typeL exprL symbol' exprR
            Just ret -> pure $ BinaryOp ret exprL symbol' exprR
    Call _ (Variable _ name) arguments -> do
        info <- analyzeVariable name
        case info of
            VariableInfo _ typeF@(FunctionType argTypes _) -> do
                exprAs <- sequence $ zipWith analyzeExprWithHint argTypes arguments
                let typeAs = getMetadata <$> exprAs
                ret <- hoist generalize $ analyzeApply name typeAs typeF
                pure $ Call ret (Variable (varType info) name) exprAs
            VariableInfo _ _ -> do
                addError $ NotAFunction name
                lift Nothing
    Call _ expr arguments -> do
        exprE <- analyzeExpr expr
        let typeE = getMetadata exprE
            genE = unsafeCodegen' expr
        case typeE of
            FunctionType argTypes _ -> do
                exprAs <- sequence $ zipWith analyzeExprWithHint argTypes arguments
                let typeAs = getMetadata <$> exprAs
                ret <- hoist generalize $ analyzeApply genE typeAs typeE
                pure $ Call ret exprE exprAs
            _ -> do
                addError (NotAFunction genE)
                lift Nothing
    Index _ left right -> do
        exprL <- analyzeExpr left
        exprR <- analyzeExprWithHint IntegerType right
        let typeL = getMetadata exprL
        pure $ Index (unnestArray typeL) exprL exprR
    Literal _ l -> pure $ Literal (literalType l) l
    Parenthesis _ expr -> analyzeExpr expr
    Structure _ struct -> do
        s <- analyzeStructure struct
        pure $ Structure (getMetadata s) s
    UnaryOp _ symbol' expr -> do
        exprE <- analyzeExpr expr
        let typeE = getMetadata exprE
        case interactsWithUnary symbol' typeE of
            Nothing -> do
                addError (IncompatibleTypes1 symbol' (void expr))
                lift Nothing
            Just ret -> pure $ UnaryOp ret symbol' exprE
    Variable _ v -> do
        typeV <- varType <$> analyzeVariable v
        pure $ Variable typeV v

analyzeExprWithHint :: Type -> Expression metadata -> AnalyzerT Maybe (Expression Type)
analyzeExprWithHint !expectedType = \case
    Access _ left right -> do
        exprL <- analyzeExpr left
        let typeL = getMetadata exprL
        case typeL of
            RecordType fieldTypes -> do
                let fieldSearch = find (\(Field l _) -> l == right) fieldTypes
                whenNothing_ fieldSearch $ addError (NotAMember typeL right)
                case fieldSearch of
                    Nothing -> do
                        addError (NotAMember typeL right)
                        pure $ Access expectedType exprL right
                    Just (Field _ fieldType) -> pure $ Access fieldType exprL right
            _ -> addError (NotARecord typeL right) *> lift (Just $ Access typeL exprL right)
    BinaryOp _ left symbol' right -> do
        exprL <- analyzeExpr left
        let typeL = getMetadata exprL
        exprR <- analyzeExprWithHint typeL right
        let typeR = getMetadata exprR
            ret = interactsWithBinary typeL symbol' typeR
        case ret of
            Nothing -> do
                addError (IncompatibleTypes2 (void left) symbol' (void right))
                pure $ BinaryOp typeL exprL symbol' exprR
            Just ret' -> pure $ BinaryOp ret' exprL symbol' exprR
    Call _ (Variable _ name) arguments -> do
        info <- analyzeVariable name
        case info of
            VariableInfo _ typeF@(FunctionType argTypes _) -> do
                exprAs <- sequence (zipWith analyzeExprWithHint argTypes arguments)
                let typeAs = getMetadata <$> exprAs
                typeRet <- hoist generalize $ analyzeApply name typeAs typeF
                pure $ Call typeRet (Variable (varType info) name) exprAs
            VariableInfo _ _ -> addError (NotAFunction name) *> lift Nothing
    Call _ expr arguments -> do
        exprE <- analyzeExpr expr
        let typeE = getMetadata exprE
            genE = unsafeCodegen' expr
        case typeE of
            FunctionType argTypes _ -> do
                exprAs <- sequence (zipWith analyzeExprWithHint argTypes arguments)
                let typeAs = getMetadata <$> exprAs
                typeRet <- hoist generalize $ analyzeApply genE typeAs typeE
                pure $ Call typeRet exprE exprAs
            _ -> addError (NotAFunction genE) *> lift Nothing
    Index _ left right -> do
        exprL <- analyzeExprWithHint (ArrayType expectedType) left
        exprR <- analyzeExprWithHint IntegerType right
        let typeL = getMetadata exprL
        pure $ Index (unnestArray typeL) exprL exprR
    Literal _ l -> do
        let typeL = literalType l
        when (typeL /= expectedType) $
            addError (TypeMismatch (unsafeCodegen' $ Literal () l) expectedType typeL)
        pure $ Literal typeL l
    Parenthesis _ expr -> analyzeExprWithHint expectedType expr
    Structure _ struct -> do
        s <- analyzeStructureWithHint expectedType struct
        let typeS = getMetadata s
        when (typeS /= expectedType) $
            addError (TypeMismatch (unsafeCodegen' struct) expectedType typeS)
        pure $ Structure (getMetadata s) s
    UnaryOp _ symbol' expr -> do
        exprE <- analyzeExprWithHint expectedType expr
        let typeE = getMetadata exprE
        case interactsWithUnary symbol' typeE of
            Nothing -> do
                addError (IncompatibleTypes1 symbol' (void expr))
                pure $ UnaryOp expectedType symbol' exprE
            Just ret -> pure $ UnaryOp ret symbol' exprE
    Variable _ v -> do
        typeV <- varType <$> analyzeVariable v
        when (typeV /= expectedType) $
            addError (TypeMismatch v expectedType typeV)
        pure $ Variable typeV v

analyzeApply :: Name -> [Type] -> Type -> Analyzer Type
analyzeApply name arguments (FunctionType argTypes ret) = do
    let nArgs = length arguments
        nTypes = length argTypes
    when (nArgs /= nTypes) $ addError $ IncompatibleSignatures name nArgs nTypes
    traverse_ (uncurry (checkTypes name)) $ zip argTypes arguments
    pure ret
analyzeApply name _ type' = addError (NotAFunction name) *> pure type'

markUsed :: VariableInfo -> VariableInfo
markUsed info = info { unused = False }

analyzeVariable :: Name -> AnalyzerT Maybe VariableInfo
analyzeVariable name = do
    (infoMaybe, symbols) <- gets $
        Map.updateLookupWithKey (const (Just . markUsed)) name . analyzerSymbols
    modify \s -> s { analyzerSymbols = symbols }
    whenNothing_ infoMaybe $ addError (UndefinedVariable name)
    lift infoMaybe

analyzeArray :: [Expression metadata] -> AnalyzerT Maybe (Structure Type)
analyzeArray [] = lift Nothing
analyzeArray (x : xs) = do
    xExpr <- analyzeExpr x
    let xType = getMetadata xExpr
    xExprs <- traverse (analyzeExprWithHint xType) xs
    pure $ Array xType (xExpr : xExprs)

-- FIXME: Stil not properly typechecking!
analyzeArrayWithHint :: Type -> [Expression metadata] -> AnalyzerT Maybe (Structure Type)
analyzeArrayWithHint expectedType [] = pure $ Array (ArrayType $ unnestArray expectedType) []
analyzeArrayWithHint expectedType (x : xs) = do
    let expectedType' = unnestArray expectedType
    xExpr <- analyzeExprWithHint expectedType' x
    xExprs <- traverse (analyzeExprWithHint expectedType') xs
    let xType = getMetadata xExpr
    when (xType == expectedType) $
        addError $ TypeMismatch (unsafeCodegen' x) (ArrayType expectedType) xType
    pure $ Array (ArrayType xType) (xExpr : xExprs)

unnestArray :: Type -> Type
unnestArray (ArrayType typeA) = typeA
unnestArray type'             = type'

-- TODO: Add types to error?
analyzeDuplicates :: Name -> [Name] -> Analyzer [Name]
analyzeDuplicates origin names =
    traverse_ (addError . DuplicateRecord origin . fst) dups *> pure sorted
  where
    sorted = sort names
    dups = filter (uncurry (==)) $ zip sorted (drop 1 sorted)

analyzeRecordFields :: [Name] -> [Field] -> Name -> [(Name, Expression metadata)] -> AnalyzerT Maybe (Structure Type)
analyzeRecordFields sortedFields templateFields name fields = do
    -- Check if number of fields match:
    let (names, exprs) = unzip fields
        (templateNames, templateTypes) = unzip $ fmap (\(Field n f) -> (n, f)) templateFields
        templateSorted = sort templateNames
    when (templateSorted /= sortedFields) $
        addError $ IncompatibleRecord name templateSorted sortedFields
    exprsWithMetadata <- sequence $ zipWith analyzeExprWithHint templateTypes exprs
    pure $ Record (RecordType templateFields) name (zip names exprsWithMetadata)

analyzeRecord :: Name -> [(Name, Expression metadata)] -> AnalyzerT Maybe (Structure Type)
analyzeRecord name fields = do
    let (names, exprs) = unzip fields
    sorted <- hoist generalize $ analyzeDuplicates name names

    recs <- asks $ recordTemplates . environment
    case Map.lookup name recs of
        Nothing -> do
            addError $ NoSuchRecord name
            exprsWithMetadata <- traverse analyzeExpr exprs
            let types = getMetadata <$> exprsWithMetadata
            pure $ Record (RecordType (zipWith Field names types)) name (zip names exprsWithMetadata)
        Just templateFields ->
            analyzeRecordFields sorted templateFields name fields

analyzeRecordWithHint :: [Field] -> Name -> [(Name, Expression metadata)] -> AnalyzerT Maybe (Structure Type)
analyzeRecordWithHint expectedFields name fields = do
    let (names, exprs) = unzip fields
    sorted <- hoist generalize $ analyzeDuplicates name names

    recs <- asks $ recordTemplates . environment
    case Map.lookup name recs of
        Nothing -> do
            addError $ NoSuchRecord name
            exprsWithMetadata <- sequence $ zipWith analyzeExprWithHint (fieldType <$> expectedFields) exprs
            let types = getMetadata <$> exprsWithMetadata
            pure $ Record (RecordType (zipWith Field names types)) name (zip names exprsWithMetadata)
        Just templateFields -> do
            analyzeRecordFields sorted templateFields name fields

analyzeAlgebraic :: Name -> [Expression metadata] -> AnalyzerT Maybe (Structure Type)
analyzeAlgebraic name exprs = do
    adts <- asks $ adtTemplates . environment
    case findMap findAdt $ Map.toList adts of
        Nothing -> do
            addError $ NoSuchConstructor name
            traverse_ analyzeExpr exprs
            lift Nothing
        Just (adtName, adtConstructors, adtConstructor) -> do
            -- TODO: Check whether exprs and adtConstructors have the same length.
            fields <- sequence $ zipWith analyzeExprWithHint (constructorTypes adtConstructor) exprs
            pure $ Algebraic (AlgebraicType adtName adtConstructors) name fields
  where
    findAdt (adtName, adtConstructors) =
        (adtName, adtConstructors,) <$> find ((== name) . constructorName) adtConstructors

literalType :: Literal -> Type
literalType = \case
    Char _    -> CharType
    Double _  -> DoubleType
    Integer _ -> IntegerType
    Text _    -> TextType

analyzeStructure :: Structure metadata -> AnalyzerT Maybe (Structure Type)
analyzeStructure = \case
    Algebraic _ name fields -> analyzeAlgebraic name fields
    Array _ elements        -> analyzeArray elements
    Record _ name fields    -> analyzeRecord name fields

analyzeStructureWithHint :: Type -> Structure metadata -> AnalyzerT Maybe (Structure Type)
analyzeStructureWithHint expectedType = \case
    Algebraic _ name fields -> analyzeAlgebraic name fields
    Array _ elements        -> analyzeArrayWithHint expectedType elements
    Record _ name fields    -> case expectedType of
        RecordType fieldTypes -> analyzeRecordWithHint fieldTypes name fields
        _                     -> analyzeRecord name fields

isNumeric :: Type -> Bool
isNumeric = \case
    DoubleType  -> True
    IntegerType -> True
    _           -> False

isText :: Type -> Bool
isText = \case
    CharType -> True
    TextType -> True
    _        -> False

interactsWithUnary :: UnarySymbol -> Type -> Maybe Type
interactsWithUnary Negate = \case
    DoubleType  -> Just DoubleType
    IntegerType -> Just IntegerType
    _           -> Nothing
interactsWithUnary Not = \case
    IntegerType -> Just IntegerType
    other
        | other == boolType -> Just boolType
        | otherwise         -> Nothing

interactsWithBinary :: Type -> BinarySymbol -> Type -> Maybe Type
interactsWithBinary TextType Add TextType = Just TextType  -- Concatenate strings.
interactsWithBinary l s r
    | l /= r                        = Nothing  -- Different types never interact.
    | l == boolType && isLogical s  = Just boolType
    | isArithmetic s && isNumeric l = Just l  -- Numeric types interact with all arithmetic operators.
    | isComparison s                = Just boolType  -- Comparisons always return booleans.
    | otherwise                     = Nothing
