module Language.LowCode.Logic.Analyzer
    ( AnalyzerReader (..)
    , AnalyzerWriter (..)
    , AnalyzerState (..)
    , Analyzer
    , runAnalyzer
    , evalAnalyzer
    , execAnalyzer
    , runAnalyzerT
    , evalAnalyzerT
    , execAnalyzerT
    , analyze
    , analyzeExpr
    , analyzeExprWithHint
    , isNumeric
    , isText
    , interactsWithUnary
    , interactsWithBinary
    ) where

import           Universum hiding (Type, find)

import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Algebra.Graph.AdjacencyMap.Algorithm as AM.Algorithm
import           Control.Monad.Trans.Chronicle
import           Data.Default.Class
import           Data.Foldable (find)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.These

import           Language.Codegen (unsafeCodegen')
import           Language.Common
import           Language.LowCode.Logic.AST
import           Language.LowCode.Logic.Error
import           Language.LowCode.Logic.Module
import           Utility

data VariableInfo = VariableInfo
    { unused  :: !Bool
    , varType :: !Type
    } deriving (Show)

mkInfo :: Type -> VariableInfo
mkInfo = VariableInfo True

data AnalyzerReader exprMetadata astMetadata = AnalyzerReader
    { currentModule :: Module exprMetadata astMetadata
    , returnType    :: Type
    }

data AnalyzerWriter = AnalyzerWriter
    { errors   :: ![(Name, Error)]
    , warnings :: ![(Name, Warning)]
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

type AnalyzerT exprMetadata astMetadata m =
    ChronicleT AnalyzerWriter (ReaderT (AnalyzerReader exprMetadata astMetadata) (StateT AnalyzerState m))

type Analyzer exprMetadata astMetadata = AnalyzerT exprMetadata astMetadata Identity

runAnalyzer
    :: Analyzer exprMetadata astMetadata a
    -> AnalyzerReader exprMetadata astMetadata
    -> AnalyzerState
    -> (These AnalyzerWriter a, AnalyzerState)
runAnalyzer m r s = runIdentity $ runAnalyzerT m r s

evalAnalyzer
    :: Analyzer exprMetadata astMetadata a
    -> AnalyzerReader exprMetadata astMetadata
    -> AnalyzerState
    -> (These AnalyzerWriter a)
evalAnalyzer m r s = runIdentity $ evalAnalyzerT m r s

execAnalyzer
    :: Analyzer exprMetadata astMetadata a
    -> AnalyzerReader exprMetadata astMetadata
    -> AnalyzerState
    -> AnalyzerState
execAnalyzer m r s = runIdentity $ execAnalyzerT m r s

runAnalyzerT
    :: AnalyzerT exprMetadata astMetadata m a
    -> AnalyzerReader exprMetadata astMetadata
    -> AnalyzerState
    -> m (These AnalyzerWriter a, AnalyzerState)
runAnalyzerT m r s = flip runStateT s $ flip runReaderT r $ runChronicleT m

evalAnalyzerT
    :: (Monad m)
    => AnalyzerT exprMetadata astMetadata m a
    -> AnalyzerReader exprMetadata astMetadata
    -> AnalyzerState
    -> m (These AnalyzerWriter a)
evalAnalyzerT m r s = flip evalStateT s $ flip runReaderT r $ runChronicleT m

execAnalyzerT
    :: (Monad m)
    => AnalyzerT exprMetadata astMetadata m a
    -> AnalyzerReader exprMetadata astMetadata
    -> AnalyzerState
    -> m AnalyzerState
execAnalyzerT m r s = flip execStateT s $ flip runReaderT r $ runChronicleT m

withAnalyzerT
    :: (AnalyzerReader exprMetadata astMetadata -> AnalyzerReader exprMetadata astMetadata)
    -> (AnalyzerState -> AnalyzerState)
    -> AnalyzerT exprMetadata astMetadata m a
    -> AnalyzerT exprMetadata astMetadata m a
withAnalyzerT fr fs at =
    ChronicleT let rt = runChronicleT at in
        ReaderT \r -> let st = runReaderT rt (fr r) in
            StateT \s' -> runStateT st (fs s')

withScope :: (Monad m) => AnalyzerT exprMetadata astMetadata m a -> AnalyzerT exprMetadata astMetadata m a
withScope action = do
    symbols <- gets analyzerSymbols
    ret <- action
    currentMod <- asks $ moduleName . currentModule
    newSymbols <- flip Map.difference symbols <$> gets analyzerSymbols
    modify \s -> s { analyzerSymbols = symbols }
    let unused = Map.foldrWithKey (appendUnused currentMod) [] newSymbols
    when (not $ null unused) $ dictate $ mempty { warnings = unused }
    pure ret
  where
    appendUnused currentMod var info acc
        | unused info = (currentMod, UnusedVariable var) : acc
        | otherwise   = acc

currentModuleName :: (Monad m) => AnalyzerT exprMetadata astMetadata m Name
currentModuleName = asks (moduleName . currentModule)

newError :: Name -> Error -> AnalyzerWriter
newError moduleName e = mempty { errors = [(moduleName, e)] }

addError :: (Monad m) => Error -> AnalyzerT exprMetadata astMetadata m ()
addError e = currentModuleName >>= \name -> dictate $ newError name e

fatalError :: (Monad m) => Error -> AnalyzerT exprMetadata astMetadata m a
fatalError e = currentModuleName >>= \name -> confess $ newError name e

newWarning :: Name -> Warning -> AnalyzerWriter
newWarning moduleName w = mempty { warnings = [(moduleName, w)] }

addWarning :: (Monad m) => Warning -> AnalyzerT exprMetadata astMetadata m ()
addWarning w = currentModuleName >>= \name -> dictate $ newWarning name w

analyzeAssign
    :: Expression metadata
    -> Expression metadata
    -> Analyzer e a (Expression Type, Expression Type)
analyzeAssign left right = do
    exprL <- analyzeExpr left
    let typeL = getMetadata exprL
    exprR <- analyzeExprWithHint typeL right
    let typeR = getMetadata exprR
    when (typeL /= typeR) $
        addError $ TypeMismatch (unsafeCodegen' left <> " = " <> unsafeCodegen' right) typeL typeR
    pure (exprL, exprR)

analyzeVar :: Name -> Type -> Expression metadata -> Analyzer e a (Expression Type)
analyzeVar name type' expr = do
    symbols <- gets analyzerSymbols
    exprR <- analyzeExprWithHint type' expr
    if Map.member name symbols
        then addError (ShadowedVariable name)
        else do
            checkTypes name type' (getMetadata exprR)
            let info = mkInfo type'
            modify \s -> s { analyzerSymbols = Map.insert name info (analyzerSymbols s) }
    pure exprR

checkTypes :: Text -> Type -> Type -> Analyzer e a ()
checkTypes name expectedType actualType
    | expectedType == actualType = pure ()
    | otherwise = addError $ TypeMismatch name expectedType actualType

-- TODO (Optional): Check for unused modules.
-- TODO: Maybe rewrite this algorithm to remove dependency on algebraic-graphs?
linkModules :: [Module e a] -> Analyzer e a (Map Name (ModuleImports e a))
linkModules mods = do
    traverse_ (addError . DuplicateModule . moduleName) repeated
    whenLeft (AM.Algorithm.topSort graph) (fatalError . CyclicImports)
    forest mods
  where
    (repeated, adj) = first catMaybes $ foldr go ([], Map.empty) mods
      where
        go m (repeatedMods, acc) =
            first (: repeatedMods) $ Map.insertLookupWithKey (\_ n _ -> n) (moduleName m) m acc

    graph = AM.fromAdjacencySets $ map (moduleName &&& Set.fromList . importedModules) mods

    tree m = do
        traverse_ (addError . NoSuchRecord) missing
        imports <- forest modules
        pure (moduleName m, ModuleImports imports m)
      where
        (missing, modules) = partitionEithers moduleLookups
        moduleLookups = map lookupModule (importedModules m)
        lookupModule name = maybe (Left name) Right (Map.lookup name adj)

    forest ms = do
        trees <- traverse tree ms
        pure $ Map.fromList trees

-- TODO: Unions with modules.
collect :: Module exprMetadata astMetadata -> Map Name VariableInfo
collect mod' = foldr mkSymbol (fmap mkInfo (externs mod')) $ functions mod'
  where
    mkSymbol (Function _ name ret _ _) acc = Map.insert name (mkInfo ret) acc

algebraicToFunctions :: Name -> Map Name Constructor -> Map Name VariableInfo
algebraicToFunctions adtName constructors = Map.map fromConstructor constructors
  where
    adtType = AlgebraicType adtName (Map.elems constructors)
    fromConstructor (Constructor _ cTypes) = mkInfo (FunctionType cTypes adtType)

--algebraicToFunctions :: (Default a) => Name -> Map Name Constructor -> Map Name (Function Type a)
--algebraicToFunctions adtName constructors = Map.map fromConstructor constructors
--  where
--    adtType = AlgebraicType adtName (Map.elems constructors)
--    fromConstructor (Constructor cName cTypes) =
--        Function def cName (FunctionType cTypes adtType) mkNames mkReturn
--      where
--        mkReturn = Return def (Just mkStructure)
--        mkStructure = Structure adtType (Algebraic adtType cName mkVariables)
--        mkNames = Text.cons '_' <$> (show <$> [0 .. length cTypes - 1])
--        mkVariables = zipWith Variable cTypes mkNames

analyze' :: Module e a -> Map Name (ModuleImports e a) -> Analyzer e a (ModuleImports Type a)
analyze' root mods = do
    mods' <- traverse (\(ModuleImports imports root') -> analyze' root' imports) (Map.elems mods)
    funcs <- withAnalyzerT
        (\r -> r { currentModule = root })
        (\s -> s { analyzerSymbols = collect root })
        (traverse (withScope . analyzeFunction) $ functions root)
    pure ModuleImports
        { moduleImports = Map.fromList $ zip (Map.keys mods) mods'
        , rootModule    = root { functions = funcs }
        }

analyze :: [Module e a] -> Analyzer e a (ModuleImports Type a, Map Name (Module Type a))
analyze mods = do
    root <- asks currentModule
    linked <- linkModules mods
    tree <- analyze' root linked
    pure (tree, getMods tree)
  where
    mkMap m = Map.singleton (moduleName m) m
    getMods (ModuleImports ms m) = Map.foldr go (mkMap m) ms
    go (ModuleImports ms m) acc = Map.unions (mkMap m : acc : (getMods <$> Map.elems ms))

analyzeFunction :: Function e a -> Analyzer e a (Function Type a)
analyzeFunction (Function _ name (FunctionType argTypes ret) arguments next) = do
    let nArgs = length arguments
        nTypes = length argTypes
    when (nArgs /= nTypes) $
        addError $ IncompatibleSignatures name nArgs nTypes
    let argsInfo = Map.fromList $ zipWith (\n t -> (n, mkInfo t)) arguments argTypes
    ast <- withAnalyzerT
        (\r -> r { returnType = ret })
        (\s -> s { analyzerSymbols = Map.union argsInfo (analyzerSymbols s) })
        (analyzeImpl next)
    pure $ Function (getMetadata ast) name (FunctionType argTypes ret) arguments ast
analyzeFunction (Function _ name _ _ _) = fatalError (NotAFunction name)

analyzeReturn :: Maybe (Expression e) -> Analyzer e a (Expression Type)
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

findAdt :: Name -> Analyzer e a (Maybe (Name, [Constructor], Constructor))
findAdt cName = do
    adts <- asks (adtTemplates . currentModule)
    pure $ findMap go $ Map.toList adts
  where
    go (adtName, adtConstructors) =
        (adtName, adtConstructors,) <$> find ((== cName) . constructorName) adtConstructors

findRecord :: Name -> Analyzer e a (Maybe [Field])
findRecord name = do
    recs <- asks (recordTemplates . currentModule)
    pure $ Map.lookup name recs

analyzePattern :: Type -> MatchPattern -> Analyzer e a (Map Name VariableInfo)
analyzePattern expectedType = \case
    AlgebraicPattern cName fields -> findAdt cName >>= \case
        Just (adtName, constructors, Constructor _cName cTypes)
            | AlgebraicType adtName constructors == expectedType ->
                Map.unions <$> sequence (zipWith analyzePattern cTypes fields)
            | otherwise -> mismatch cName (AlgebraicType adtName constructors)
        Nothing -> addError (NoSuchConstructor cName) *> pure Map.empty
    ArrayPattern positions -> case expectedType of
        ArrayType t -> Map.unions <$> traverse (analyzePattern t) positions
        _           -> addError UnknownArray *> pure Map.empty
    LiteralPattern lit
        | literalType lit == expectedType -> do
            when (expectedType == DoubleType) $
                addWarning (FloatingPointEquality (getDouble lit))
            pure Map.empty
        | otherwise -> mismatch (unsafeCodegen' lit) (literalType lit)
    NamePattern name -> pure (Map.singleton name (mkInfo expectedType))
    RecordPattern name fields -> findRecord name >>= \case
        Just fields'
            | RecordType fields' == expectedType ->
                Map.unions <$> sequence (zipWith analyzePattern (fieldType <$> fields') fields)
            | otherwise -> mismatch name (RecordType fields')
        Nothing -> addError (NoSuchConstructor name) *> pure Map.empty
  where
    mismatch name actualType = do
        addError (TypeMismatch name expectedType actualType)
        pure Map.empty

    getDouble (Double d) = d
    getDouble _          = error "Panic: error in analyzePattern.getDouble. This is likely a bug. Please report it."

-- TODO: Check for non-exhaustive patterns.
-- TODO: Check for repeated patterns.
-- TODO: Check for unreachable patterns.
analyzeMatch :: Expression e -> [(MatchPattern, AST e a)] -> Analyzer e a (Expression Type, [(MatchPattern, AST Type a)])
analyzeMatch expr branches = do
    exprWithMetadata <- analyzeExpr expr
    branches' <- traverse (go exprWithMetadata []) branches
    pure (exprWithMetadata, branches')
  where
    go e _patterns (p, a) = withScope do
        infos <- analyzePattern (getMetadata e) p
        modify \s -> s { analyzerSymbols = Map.union infos (analyzerSymbols s) }
        ast' <- analyzeImpl a
        pure (p, ast')

analyzeImpl :: AST e a -> Analyzer e a (AST Type a)
analyzeImpl = \case
    Assign m left right next -> do
        (left', right') <- analyzeAssign left right
        Assign m left' right' <$> analyzeImpl next
    End m -> pure $ End m
    Expression m expr next -> Expression m <$> analyzeExpr expr <*> analyzeImpl next
    If m cond true false next -> do
        cond' <- analyzeExprWithHint boolType cond
        checkTypes "if" boolType (getMetadata cond')
        true'  <- withScope $ analyzeImpl true
        false' <- withScope $ analyzeImpl false
        If m cond' true' false' <$> analyzeImpl next
    Match m expr branches next -> do
        (expr', branches') <- analyzeMatch expr branches
        Match m expr' branches' <$> analyzeImpl next
    Return m exprMaybe -> Return m <$> (Just <$> analyzeReturn exprMaybe)
    Var m var type' expr next -> do
        Var m var type' <$> analyzeVar var type' expr <*> analyzeImpl next
    While m cond body next -> do
        cond' <- analyzeExprWithHint boolType cond
        checkTypes "while" boolType (getMetadata cond')
        body' <- withScope $ analyzeImpl body
        While m cond' body' <$> analyzeImpl next

analyzeExpr :: Expression metadata -> Analyzer e a (Expression Type)
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
                ret <- analyzeApply name typeAs typeF
                pure $ Call ret (Variable (varType info) name) exprAs
            VariableInfo _ _ -> fatalError $ NotAFunction name
    Call _ expr arguments -> do
        exprE <- analyzeExpr expr
        let typeE = getMetadata exprE
            genE = unsafeCodegen' expr
        case typeE of
            FunctionType argTypes _ -> do
                exprAs <- sequence $ zipWith analyzeExprWithHint argTypes arguments
                let typeAs = getMetadata <$> exprAs
                ret <- analyzeApply genE typeAs typeE
                pure $ Call ret exprE exprAs
            _ -> fatalError (NotAFunction genE)
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
                fatalError (IncompatibleTypes1 symbol' (void expr))
                -- TODO: Analyze whether cases like this should return an error or not!
                --pure $ UnaryOp typeE symbol' exprE
            Just ret -> pure $ UnaryOp ret symbol' exprE
    Variable _ v -> do
        typeV <- varType <$> analyzeVariable v
        pure $ Variable typeV v

analyzeExprWithHint :: Type -> Expression metadata -> Analyzer e a (Expression Type)
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
            _ -> do
                addError (NotARecord typeL right)
                pure $ Access expectedType exprL right
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
                typeRet <- analyzeApply name typeAs typeF
                pure $ Call typeRet (Variable (varType info) name) exprAs
            VariableInfo _ _ -> do
                -- TODO: Add type to error?
                addError (NotAFunction name)
                exprAs <- traverse analyzeExpr arguments
                pure $ Call expectedType (Variable (varType info) name) exprAs
    Call _ expr arguments -> do
        exprE <- analyzeExpr expr
        let typeE = getMetadata exprE
            genE = unsafeCodegen' expr
        case typeE of
            FunctionType argTypes _ -> do
                exprAs <- sequence (zipWith analyzeExprWithHint argTypes arguments)
                let typeAs = getMetadata <$> exprAs
                typeRet <- analyzeApply genE typeAs typeE
                pure $ Call typeRet exprE exprAs
            _ -> do
                addError (NotAFunction genE)
                exprAs <- traverse analyzeExpr arguments
                pure $ Call expectedType exprE exprAs
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

analyzeApply :: Name -> [Type] -> Type -> Analyzer e a Type
analyzeApply name arguments (FunctionType argTypes ret) = do
    let nArgs = length arguments
        nTypes = length argTypes
    when (nArgs /= nTypes) $ addError $ IncompatibleSignatures name nArgs nTypes
    traverse_ (uncurry (checkTypes name)) $ zip argTypes arguments
    pure ret
analyzeApply name _ type' = addError (NotAFunction name) *> pure type'

markUsed :: VariableInfo -> VariableInfo
markUsed info = info { unused = False }

analyzeVariable :: Name -> Analyzer e a VariableInfo
analyzeVariable name = do
    (infoMaybe, symbols) <- gets $
        Map.updateLookupWithKey (const (Just . markUsed)) name . analyzerSymbols
    modify \s -> s { analyzerSymbols = symbols }
    maybe (fatalError (UndefinedVariable name)) pure infoMaybe

analyzeArray :: [Expression metadata] -> Analyzer e a (Structure Type)
analyzeArray [] = fatalError UnknownArray
analyzeArray (x : xs) = do
    xExpr <- analyzeExpr x
    let xType = getMetadata xExpr
    xExprs <- traverse (analyzeExprWithHint xType) xs
    pure $ Array xType (xExpr : xExprs)

-- FIXME: Stil not properly typechecking!
analyzeArrayWithHint :: Type -> [Expression metadata] -> Analyzer e a (Structure Type)
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
analyzeDuplicates :: Name -> [Name] -> Analyzer e a [Name]
analyzeDuplicates origin names =
    traverse_ (addError . DuplicateRecord origin . fst) dups *> pure sorted
  where
    sorted = sort names
    dups = filter (uncurry (==)) $ zip sorted (drop 1 sorted)

analyzeRecordFields :: [Name] -> [Field] -> Name -> [(Name, Expression metadata)] -> Analyzer e a (Structure Type)
analyzeRecordFields sortedFields templateFields name fields = do
    -- Check if number of fields match:
    let (names, exprs) = unzip fields
        (templateNames, templateTypes) = unzip $ fmap (\(Field n f) -> (n, f)) templateFields
        templateSorted = sort templateNames
    when (templateSorted /= sortedFields) $
        addError $ IncompatibleRecord name templateSorted sortedFields
    exprsWithMetadata <- sequence $ zipWith analyzeExprWithHint templateTypes exprs
    pure $ Record (RecordType templateFields) name (zip names exprsWithMetadata)

analyzeRecord :: Name -> [(Name, Expression metadata)] -> Analyzer e a (Structure Type)
analyzeRecord name fields = do
    let (names, exprs) = unzip fields
    sorted <- analyzeDuplicates name names

    fieldsLookup <- findRecord name
    case fieldsLookup of
        Nothing -> do
            addError $ NoSuchRecord name
            exprsWithMetadata <- traverse analyzeExpr exprs
            let types = getMetadata <$> exprsWithMetadata
            pure $ Record (RecordType (zipWith Field names types)) name (zip names exprsWithMetadata)
        Just templateFields -> analyzeRecordFields sorted templateFields name fields

analyzeRecordWithHint :: [Field] -> Name -> [(Name, Expression metadata)] -> Analyzer e a (Structure Type)
analyzeRecordWithHint expectedFields name fields = do
    let (names, exprs) = unzip fields
    sorted <- analyzeDuplicates name names

    fieldsLookup <- findRecord name
    case fieldsLookup of
        Nothing -> do
            addError $ NoSuchRecord name
            exprsWithMetadata <- sequence $ zipWith analyzeExprWithHint (fieldType <$> expectedFields) exprs
            let types = getMetadata <$> exprsWithMetadata
            pure $ Record (RecordType (zipWith Field names types)) name (zip names exprsWithMetadata)
        Just templateFields -> analyzeRecordFields sorted templateFields name fields

analyzeAlgebraic :: Name -> [Expression metadata] -> Analyzer e a (Structure Type)
analyzeAlgebraic name exprs = findAdt name >>= \case
    Nothing -> do
        traverse_ analyzeExpr exprs
        fatalError $ NoSuchConstructor name
    Just (adtName, adtConstructors, adtConstructor) -> do
        -- TODO: Check whether exprs and adtConstructors have the same length.
        fields <- sequence $ zipWith analyzeExprWithHint (constructorTypes adtConstructor) exprs
        pure $ Algebraic (AlgebraicType adtName adtConstructors) name fields

literalType :: Literal -> Type
literalType = \case
    Char _    -> CharType
    Double _  -> DoubleType
    Integer _ -> IntegerType
    Text _    -> TextType

analyzeStructure :: Structure metadata -> Analyzer e a (Structure Type)
analyzeStructure = \case
    Algebraic _ name fields -> analyzeAlgebraic name fields
    Array _ elements        -> analyzeArray elements
    Record _ name fields    -> analyzeRecord name fields

analyzeStructureWithHint :: Type -> Structure metadata -> Analyzer e a (Structure Type)
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
