module Language.LowCode.Logic.Analyzer
    ( AnalyzerWriter (..)
    , AnalyzerState (..)
    , Analyzer
    , runAnalyzer
    , evalAnalyzer
    , execAnalyzer
    , runAnalyzerT
    , evalAnalyzerT
    , execAnalyzerT
    , analyze
    , analyze'
    , analyzeExpr
    , analyzeExprWithHint
    , isNumeric
    , isText
    , interactsWithUnary
    , interactsWithBinary
    ) where

import           Universum hiding (Type)

import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Algebra.Graph.AdjacencyMap.Algorithm as AM.Algorithm
import           Control.Monad.Trans.Chronicle
import           Data.Default.Class
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.These
import qualified Data.Traversable as Traversable

import           Language.Codegen (unsafeCodegen')
import           Language.Common
import           Language.LowCode.Logic.AST
import           Language.LowCode.Logic.Error
import           Language.LowCode.Logic.Module
import           Language.LowCode.Logic.Standard.Prelude (boolType, unit', unitType)
import           Language.LowCode.Logic.Structure
import           Language.LowCode.Logic.Type
import           Utility

data VariableInfo = VariableInfo
    { unused  :: !Bool
    , varName :: !Name
    , varType :: !Type
    } deriving (Show)

mkInfo :: Name -> Type -> VariableInfo
mkInfo = VariableInfo True

-- TODO: Add language specific analysis, such as JS variable name checking.
data AnalyzerReader = AnalyzerReader

instance Default AnalyzerReader where
    def = AnalyzerReader

data AnalyzerWriter = AnalyzerWriter
    { errors   :: !(Map Name [Error])
    , warnings :: !(Map Name [Warning])
    } deriving (Show)

instance Semigroup AnalyzerWriter where
    AnalyzerWriter e w <> AnalyzerWriter e' w' = AnalyzerWriter (e <> e') (w <> w')

instance Monoid AnalyzerWriter where
    mempty = AnalyzerWriter Map.empty Map.empty

instance Default AnalyzerWriter where
    def = mempty

data ModuleImports exprMetadata = ModuleImports
    { cachedSymbols :: !(Map Name VariableInfo)
    , moduleImports :: !(Map Name (ModuleImports exprMetadata))
    , rootModule    :: !(Module exprMetadata)
    } deriving (Show)

--instance Semigroup (ModuleImports e a) where
--    ModuleImports ca cs ci rm <> ModuleImports ca' cs' ci' rm' =
--        ModuleImports (cs <> ca') (cs <> cs') (ci <> ci') (rm <> rm') (joinMods rm rm')
--      where
--        joinMods (Module at e f im mn) (Module at' e' f' im' mn') =
--            Module (at <> at') (e <> e') (f <> f') (im <> im') (mn <> mn')

data AnalyzerState exprMetadata = AnalyzerState
    { analyzerSymbols      :: !(Map Name VariableInfo)
    , currentModule        :: !(Module exprMetadata)
    , adtsInScope          :: !(Map Name [Constructor Type])
    , modulesAnalyzedSoFar :: !(Map Name (ModuleImports Type))
    , returnType           :: !Type
    } deriving (Show)

instance Default (AnalyzerState exprMetadata) where
    def = AnalyzerState Map.empty (mkModule "") Map.empty Map.empty unitType

type AnalyzerT exprMetadata m
    = ChronicleT AnalyzerWriter (ReaderT AnalyzerReader (StateT (AnalyzerState exprMetadata) m))

type Analyzer exprMetadata = AnalyzerT exprMetadata Identity

runAnalyzer
    :: Analyzer exprMetadata a
    -> AnalyzerReader
    -> AnalyzerState exprMetadata
    -> (These AnalyzerWriter a, AnalyzerState exprMetadata)
runAnalyzer m r s = runIdentity $ runAnalyzerT m r s

evalAnalyzer
    :: Analyzer exprMetadata a
    -> AnalyzerReader
    -> AnalyzerState exprMetadata
    -> (These AnalyzerWriter a)
evalAnalyzer m r s = runIdentity $ evalAnalyzerT m r s

execAnalyzer
    :: Analyzer exprMetadata a
    -> AnalyzerReader
    -> AnalyzerState exprMetadata
    -> AnalyzerState exprMetadata
execAnalyzer m r s = runIdentity $ execAnalyzerT m r s

runAnalyzerT
    :: AnalyzerT exprMetadata m a
    -> AnalyzerReader
    -> AnalyzerState exprMetadata
    -> m (These AnalyzerWriter a, AnalyzerState exprMetadata)
runAnalyzerT m r s = flip runStateT s $ flip runReaderT r $ runChronicleT m

evalAnalyzerT
    :: (Monad m)
    => AnalyzerT exprMetadata m a
    -> AnalyzerReader
    -> AnalyzerState exprMetadata
    -> m (These AnalyzerWriter a)
evalAnalyzerT m r s = flip evalStateT s $ flip runReaderT r $ runChronicleT m

execAnalyzerT
    :: (Monad m)
    => AnalyzerT exprMetadata m a
    -> AnalyzerReader
    -> AnalyzerState exprMetadata
    -> m (AnalyzerState exprMetadata)
execAnalyzerT m r s = flip execStateT s $ flip runReaderT r $ runChronicleT m

withAnalyzerT
    :: (AnalyzerReader -> AnalyzerReader)
    -> (AnalyzerState exprMetadata -> AnalyzerState exprMetadata)
    -> AnalyzerT exprMetadata m a
    -> AnalyzerT exprMetadata m a
withAnalyzerT fr fs at =
    ChronicleT let rt = runChronicleT at in
        ReaderT \r -> let st = runReaderT rt (fr r) in
            StateT \s -> runStateT st (fs s)

withScope :: (Monad m) => AnalyzerT exprMetadata m a -> AnalyzerT exprMetadata m a
withScope action = do
    symbols <- gets analyzerSymbols
    ret <- action
    newSymbols <- flip Map.difference symbols <$> gets analyzerSymbols
    modify \s -> s { analyzerSymbols = symbols }
    currentMod <- currentModuleName
    let unused = Map.foldrWithKey (appendUnused currentMod) Map.empty newSymbols
    when (not $ null unused) $ dictate $ mempty { warnings = unused }
    pure ret
  where
    appendUnused currentMod var info acc
        | unused info = Map.insertWith (<>) currentMod [UnusedVariable var] acc
        | otherwise   = acc

currentModuleName :: (Monad m) => AnalyzerT exprMetadata m Name
currentModuleName = gets (moduleName . currentModule)

newError :: Name -> Error -> AnalyzerWriter
newError moduleName e = mempty { errors = Map.singleton moduleName [e] }

addError :: (Monad m) => Error -> AnalyzerT exprMetadata m ()
addError e = currentModuleName >>= \name -> dictate $ newError name e

fatalError :: (Monad m) => Error -> AnalyzerT exprMetadata m a
fatalError e = currentModuleName >>= \name -> confess $ newError name e

newWarning :: Name -> Warning -> AnalyzerWriter
newWarning moduleName w = mempty { warnings = Map.singleton moduleName [w] }

addWarning :: (Monad m) => Warning -> AnalyzerT exprMetadata m ()
addWarning w = currentModuleName >>= \name -> dictate $ newWarning name w

addVar :: Name -> Type -> Analyzer e' VariableInfo
addVar n t = do
    symbols <- gets analyzerSymbols
    let info = mkInfo n t
    if Map.member n symbols
        then addError (ShadowedVariable n)
        else modify \s -> s { analyzerSymbols = Map.insert n info symbols }
    pure info

addVars :: (Traversable t) => t (Name, Type) -> Analyzer e' (t VariableInfo)
addVars = Traversable.traverse (uncurry addVar)

analyzeAssign
    :: Expression e
    -> Expression e
    -> Analyzer e' (Expression Type, Expression Type)
analyzeAssign left right = do
    exprL <- analyzeExpr left
    let typeL = getMetadata exprL
    exprR <- analyzeExprWithHint typeL right
    let typeR = getMetadata exprR
    when (typeL /= typeR) $
        addError $ TypeMismatch (unsafeCodegen' left <> " = " <> unsafeCodegen' right) typeL typeR
    pure (exprL, exprR)

analyzeVar :: Name -> Type -> Expression e -> Analyzer e' (Expression Type)
analyzeVar name type' expr = do
    exprR <- analyzeExprWithHint type' expr
    checkTypes name type' (getMetadata exprR)
    addVar name type'
    pure exprR

checkTypes :: Text -> Type -> Type -> Analyzer e' ()
checkTypes name expectedType actualType
    | expectedType == actualType = pure ()
    | otherwise = addError $ TypeMismatch name expectedType actualType

-- TODO (Optional): Check for unused modules.
-- TODO: Maybe rewrite this algorithm to remove dependency on algebraic-graphs?
linkModules :: [Module e] -> Analyzer e' (Map Name (ModuleImports e))
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
        traverse_ (addError . NoSuchModule) missing
        imports <- forest modules
        pure (moduleName m, ModuleImports Map.empty imports m)
      where
        (missing, modules) = partitionEithers moduleLookups
        moduleLookups = map lookupModule (importedModules m)
        lookupModule name = maybe (Left name) Right (Map.lookup name adj)

    forest ms = do
        trees <- traverse tree ms
        pure $ Map.fromList trees

collect :: Module e -> Analyzer e' (Map Name VariableInfo)
collect mod' = do
    exts' <- addVars exts
    funcs' <- addVars funcs
    --adts <- algebraicToFunctions (adtTemplates mod')
    --pure $ Map.unions [exts', funcs', adts]
    pure $ Map.union exts' funcs'
  where
    exts = Map.mapWithKey (,) (externs mod')
    funcs = Map.fromList $ getFuncNameType <$> functions mod'
    getFuncNameType (Function n t _ _) = (n, (n, t))

analyzeImpl :: Map Name (ModuleImports e) -> Analyzer e (Map Name (ModuleImports Type))
analyzeImpl mods = forM mods \(ModuleImports _ imports root) -> do
    analyzed <- gets modulesAnalyzedSoFar
    case Map.lookup (moduleName root) analyzed of
        Just root' -> pure root'
        Nothing -> do
            mods' <- analyzeImpl imports
            let adts = Map.unions (adtTemplates root : Map.elems (map (adtTemplates . rootModule) imports))
                cache = Map.unions $ map cachedSymbols $ Map.elems mods'
            modify \s -> s
                { adtsInScope     = adts
                , analyzerSymbols = cache
                , currentModule   = root
                }
            moduleSymbols <- collect root
            funcs <- traverse (withScope . analyzeFunction) (functions root)

            let mi = ModuleImports
                    { cachedSymbols = moduleSymbols
                    , moduleImports = mods'
                    , rootModule    = root { functions = funcs }
                    }
            modify \s -> s
                { modulesAnalyzedSoFar = Map.insert (moduleName root) mi (modulesAnalyzedSoFar s)
                }
            pure mi


analyze :: NonEmpty (Module e) -> These AnalyzerWriter (Map Name (Module Type))
analyze (root :| mods) = fst $ runAnalyzer (analyze' (root : mods)) def def

analyze' :: [Module e] -> Analyzer e (Map Name (Module Type))
analyze' mods = fmap getMods . analyzeImpl =<< linkModules mods
  where
    getMods = Map.foldr go Map.empty
    go (ModuleImports _ ms m) acc = Map.insert (moduleName m) m (Map.union acc (getMods ms))

analyzeFunction :: Function e -> Analyzer e' (Function Type)
analyzeFunction (Function name (FunctionType argTypes ret) arguments body) = do
    let nArgs = length arguments
        nTypes = length argTypes
    when (nArgs /= nTypes) $
        addError $ IncompatibleSignatures name nArgs nTypes
    asts <- withScope do
        void (addVars (zip arguments argTypes))
        withAnalyzerT id (\s -> s { returnType = ret }) (analyzeAsts body)
    pure $ Function name (FunctionType argTypes ret) arguments asts
analyzeFunction (Function name _ _ _) = fatalError (NotAFunction name)

analyzeReturn :: Maybe (Expression e) -> Analyzer e' (Expression Type)
analyzeReturn Nothing = do
    ret <- gets returnType
    when (ret /= unitType) $
        addError $ TypeMismatch "return" ret unitType
    pure $ Structure unitType unit'
analyzeReturn (Just expr) = do
    ret <- gets returnType
    exprE <- analyzeExprWithHint ret expr
    let typeE = getMetadata exprE
    when (ret /= typeE) $
        addError $ TypeMismatch (unsafeCodegen' expr) ret typeE
    pure exprE

findCtor :: Name -> Analyzer e' (Maybe (Constructor Type))
findCtor cName = do
    adts <- gets adtsInScope
    pure $ findMap (find ((== cName) . constructorName)) $ Map.elems adts

analyzePattern :: Type -> MatchPattern -> Analyzer e' (Map Name VariableInfo)
analyzePattern expectedType = \case
    LiteralPattern lit
        | literalType lit == expectedType -> do
            when (expectedType == DoubleType) $
                addWarning (FloatingPointEquality (getDouble lit))
            pure Map.empty
        | otherwise -> mismatch (unsafeCodegen' lit) (literalType lit)
    NamePattern name -> pure (Map.singleton name (mkInfo name expectedType))
    StructurePattern struct -> case struct of
        Algebraic (Constructor adtName cName field) -> findCtor cName >>= \case
            Just (Constructor _adtName cName' cType)
                | AlgebraicType adtName == expectedType -> do
                    when (cName /= cName') $
                        addError (ConstructorMismatch adtName cName' cName)
                    case (field, cType) of
                        (Just field', Just cType') -> analyzePattern cType' field'
                        _                          -> pure Map.empty
                | otherwise -> mismatch cName (AlgebraicType adtName)
            Nothing -> addError (NoSuchConstructor adtName cName) *> pure Map.empty
        Array positions -> case expectedType of
            ArrayType t -> Map.unions <$> traverse (analyzePattern t) positions
            _           -> addError UnknownArray *> pure Map.empty
        Record fields -> do
            sorted <- analyzeDuplicates fields
            case expectedType of
                RecordType fields' -> do
                    sorted' <- analyzeDuplicates fields'
                    let zipped = zipWithExact analyzePattern (map fieldValue sorted') (map fieldValue sorted)
                        names = Set.fromList (map fieldName sorted)
                        names' = Set.fromList (map fieldName sorted')
                        missing = names' Set.\\ names
                        extra = names Set.\\ names'
                    traverse_ (addError . UndefinedVariable) extra
                    whenJust (nonEmpty (Set.elems missing)) (addError . MissingFields)
                    case zipped of
                        Just zipped' -> Map.unions <$> sequence zipped'
                        Nothing      -> pure Map.empty
                _ -> do
                    let expr = patternToExpr $ StructurePattern $ Record sorted
                    exprWithMetadata <- analyzeExpr expr
                    let typeE = getMetadata exprWithMetadata
                    mismatch (unsafeCodegen' expr) typeE
  where
    mismatch name actualType = do
        addError (TypeMismatch name expectedType actualType)
        pure Map.empty

    getDouble (Double d) = d
    getDouble _          = error "Panic: error in analyzePattern.getDouble. This is likely a bug. Please report it."

-- TODO: Check for non-exhaustive patterns.
-- TODO: Check for repeated patterns.
-- TODO: Check for unreachable patterns.
analyzeMatch
    :: Expression e
    -> [Branch e]
    -> Analyzer e' (Expression Type, [Branch Type])
analyzeMatch expr branches = do
    exprWithMetadata <- analyzeExpr expr
    branches' <- traverse (go exprWithMetadata) branches
    pure (exprWithMetadata, branches')
  where
    go e (Branch p as) = withScope do
        infos <- analyzePattern (getMetadata e) p
        modify \s -> s { analyzerSymbols = Map.union infos (analyzerSymbols s) }
        Branch p <$> analyzeAsts as

analyzeAsts :: [AST e] -> Analyzer e' [AST Type]
analyzeAsts = traverse analyzeAst

analyzeAst :: AST e -> Analyzer e' (AST Type)
analyzeAst = \case
    Assign left right -> do
        (left', right') <- analyzeAssign left right
        pure $ Assign left' right'
    Expression expr -> Expression <$> analyzeExpr expr
    If cond true false -> do
        cond' <- analyzeExprWithHint boolType cond
        checkTypes "if" boolType (getMetadata cond')
        true'  <- withScope $ analyzeAsts true
        false' <- withScope $ analyzeAsts false
        pure $ If cond' true' false'
    Match expr branches -> do
        (expr', branches') <- analyzeMatch expr branches
        pure $ Match expr' branches'
    Return exprMaybe -> Return <$> (Just <$> analyzeReturn exprMaybe)
    Var var type' expr -> do
        Var var type' <$> analyzeVar var type' expr
    While cond body -> do
        cond' <- analyzeExprWithHint boolType cond
        checkTypes "while" boolType (getMetadata cond')
        body' <- withScope $ analyzeAsts body
        pure $ While cond' body'

analyzeExpr :: Expression e -> Analyzer e' (Expression Type)
analyzeExpr = \case
    Access _ left right -> do
        exprL <- analyzeExpr left
        let typeL = getMetadata exprL
        case typeL of
            RecordType fieldTypes -> case find (\(Field n _) -> n == right) fieldTypes of
                Nothing -> fatalError (NotAMember typeL right)
                Just (Field _ fieldType) -> pure (Access fieldType exprL right)
            _ -> fatalError (NotARecord typeL right)
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
            VariableInfo _ _ typeF@(FunctionType argTypes _) -> do
                exprAs <- sequence $ zipWith analyzeExprWithHint argTypes arguments
                let typeAs = getMetadata <$> exprAs
                ret <- analyzeApply name typeAs typeF
                pure $ Call ret (Variable (varType info) name) exprAs
            _ -> fatalError $ NotAFunction name
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
        (typeS, s) <- analyzeStructure struct
        pure $ Structure typeS s
    UnaryOp _ symbol' expr -> do
        exprE <- analyzeExpr expr
        let typeE = getMetadata exprE
        case interactsWithUnary symbol' typeE of
            -- TODO: Analyze whether cases like this should return an error or not!
            Nothing -> fatalError (IncompatibleTypes1 symbol' (void expr))
            --Nothing -> pure (UnaryOp typeE symbol' exprE)
            Just ret -> pure $ UnaryOp ret symbol' exprE
    Variable _ v -> do
        typeV <- varType <$> analyzeVariable v
        pure $ Variable typeV v

analyzeExprWithHint :: Type -> Expression e -> Analyzer e' (Expression Type)
analyzeExprWithHint !expectedType = \case
    Access _ left right -> do
        exprL <- analyzeExpr left
        let typeL = getMetadata exprL
        case typeL of
            RecordType fieldTypes -> case find (\(Field n _) -> n == right) fieldTypes of
                Nothing -> addError (NotAMember typeL right) *> pure (Access expectedType exprL right)
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
            VariableInfo _ _ typeF@(FunctionType argTypes _) -> do
                exprAs <- sequence (zipWith analyzeExprWithHint argTypes arguments)
                let typeAs = getMetadata <$> exprAs
                typeRet <- analyzeApply name typeAs typeF
                pure $ Call typeRet (Variable (varType info) name) exprAs
            _ -> do
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
        (typeS, s) <- analyzeStructureWithHint expectedType struct
        --when (typeS /= expectedType) $
        --    addError (TypeMismatch (unsafeCodegen' struct) expectedType typeS)
        pure $ Structure typeS s
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

analyzeApply :: Name -> [Type] -> Type -> Analyzer e' Type
analyzeApply name arguments (FunctionType argTypes ret) = do
    let nArgs = length arguments
        nTypes = length argTypes
    when (nArgs /= nTypes) $ addError $ IncompatibleSignatures name nTypes nArgs
    traverse_ (uncurry (checkTypes name)) $ zip argTypes arguments
    pure ret
analyzeApply name _ type' = addError (NotAFunction name) *> pure type'

markUsed :: VariableInfo -> VariableInfo
markUsed info = info { unused = False }

analyzeVariable :: Name -> Analyzer e' VariableInfo
analyzeVariable name = do
    (infoMaybe, symbols) <- gets $
        Map.updateLookupWithKey (const (Just . markUsed)) name . analyzerSymbols
    modify \s -> s { analyzerSymbols = symbols }
    maybe (fatalError (UndefinedVariable name)) pure infoMaybe

analyzeArray :: [Expression e] -> Analyzer e' (Type, Structure (Expression Type))
analyzeArray [] = fatalError UnknownArray
analyzeArray (x : xs) = do
    xExpr <- analyzeExpr x
    let xType = getMetadata xExpr
    xExprs <- traverse (analyzeExprWithHint xType) xs
    pure (xType, Array (xExpr : xExprs))

-- FIXME: Still not properly typechecking!
analyzeArrayWithHint :: Type -> [Expression e] -> Analyzer e' (Type, Structure (Expression Type))
analyzeArrayWithHint expectedType [] = pure (ArrayType $ unnestArray expectedType, Array [])
analyzeArrayWithHint expectedType (x : xs) = do
    let expectedType' = unnestArray expectedType
    xExpr <- analyzeExprWithHint expectedType' x
    xExprs <- traverse (analyzeExprWithHint expectedType') xs
    let xType = getMetadata xExpr
    when (xType == expectedType) $
        addError $ TypeMismatch (unsafeCodegen' x) (ArrayType expectedType) xType
    pure (ArrayType xType, Array (xExpr : xExprs))

unnestArray :: Type -> Type
unnestArray (ArrayType typeA) = typeA
unnestArray type'             = type'

unzipFields :: [Field e] -> ([Name], [e])
unzipFields = unzip . map (\(Field n v) -> (n, v))

-- TODO: Add types to error?
analyzeDuplicates :: [Field e] -> Analyzer e' [Field e]
analyzeDuplicates fields = traverse_ (addError . DuplicateRecord . fst) dups *> pure sorted
  where
    sorted = sortOn fieldName fields
    sortedNames = map fieldName sorted
    dups = filter (uncurry (==)) $ zip sortedNames (drop 1 sortedNames)

analyzeRecord :: [Field (Expression e)] -> Analyzer e' (Type, Structure (Expression Type))
analyzeRecord fields = do
    sorted <- analyzeDuplicates fields
    let (names, exprs) = unzipFields sorted

    exprsWithMetadata <- traverse analyzeExpr exprs
    let types = getMetadata <$> exprsWithMetadata
    pure (RecordType (zipWith Field names types), Record (zipWith Field names exprsWithMetadata))

analyzeRecordWithHint :: [Field Type] -> [Field (Expression e)] -> Analyzer e' (Type, Structure (Expression Type))
analyzeRecordWithHint expectedFields fields = do
    sorted <- analyzeDuplicates fields

    -- Check if number of fields match and that they have the same types:
    let expectedSorted = sortOn fieldName expectedFields
        (_expectedNames, expectedTypes) = unzipFields expectedSorted
        (names, exprs) = unzipFields sorted
    exprsWithMetadata <- sequence $ zipWith analyzeExprWithHint expectedTypes exprs
    let types = getMetadata <$> exprsWithMetadata
        actualFields = zipWith Field names types
    when (expectedSorted /= actualFields) $
        addError $ TypeMismatch (unsafeCodegen' $ Record (void <<$>> sorted)) (RecordType expectedSorted) (RecordType actualFields)

    pure (RecordType actualFields, Record (zipWith Field names exprsWithMetadata))

-- TODO: analyzeAlgebraicWithHint
analyzeAlgebraic :: Constructor (Expression e) -> Analyzer e' (Type, Structure (Expression Type))
analyzeAlgebraic (Constructor adtName name expr) = findCtor name >>= \case
    Nothing -> do
        whenJust expr (void . analyzeExpr)
        fatalError $ NoSuchConstructor adtName name
    Just (Constructor _adtName name' adtType) -> do
        when (name /= name') $
            addError (ConstructorMismatch adtName name' name)
        field <- sequence $ liftA2 analyzeExprWithHint adtType expr
        pure (AlgebraicType adtName, Algebraic (Constructor adtName name field))

literalType :: Literal -> Type
literalType = \case
    Char _    -> CharType
    Double _  -> DoubleType
    Integer _ -> IntegerType
    Text _    -> TextType

analyzeStructure :: Structure (Expression e) -> Analyzer e' (Type, Structure (Expression Type))
analyzeStructure = \case
    Algebraic constructor -> analyzeAlgebraic constructor
    Array elements        -> analyzeArray elements
    Record fields         -> analyzeRecord fields

analyzeStructureWithHint :: Type -> Structure (Expression e) -> Analyzer e' (Type, Structure (Expression Type))
analyzeStructureWithHint expectedType = \case
    Algebraic constructor -> analyzeAlgebraic constructor
    Array elements        -> analyzeArrayWithHint expectedType elements
    Record fields         -> case expectedType of
        RecordType fieldNames -> analyzeRecordWithHint fieldNames fields
        _                     -> analyzeRecord fields

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
