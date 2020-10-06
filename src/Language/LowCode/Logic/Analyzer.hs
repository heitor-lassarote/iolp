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
import           Language.LowCode.Logic.Standard (boolType, unit', unitType)
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

data ModuleImports exprMetadata astMetadata = ModuleImports
    { cachedSymbols :: !(Map Text VariableInfo)
    , moduleImports :: !(Map Name (ModuleImports exprMetadata astMetadata))
    , rootModule    :: !(Module exprMetadata astMetadata)
    } deriving (Show)

data AnalyzerState exprMetadata astMetadata = AnalyzerState
    { analyzerSymbols      :: !(Map Text VariableInfo)
    , currentModule        :: !(Module exprMetadata astMetadata)
    , modulesAnalyzedSoFar :: !(Map Name (ModuleImports Type astMetadata))
    , returnType           :: !Type
    } deriving (Show)

instance Default (AnalyzerState exprMetadata astMetadata) where
    def = AnalyzerState Map.empty (mkModule "") Map.empty unitType

type AnalyzerT exprMetadata astMetadata m
    = ChronicleT AnalyzerWriter (ReaderT AnalyzerReader (StateT (AnalyzerState exprMetadata astMetadata) m))

type Analyzer exprMetadata astMetadata = AnalyzerT exprMetadata astMetadata Identity

runAnalyzer
    :: Analyzer exprMetadata astMetadata a
    -> AnalyzerReader
    -> AnalyzerState exprMetadata astMetadata
    -> (These AnalyzerWriter a, AnalyzerState exprMetadata astMetadata)
runAnalyzer m r s = runIdentity $ runAnalyzerT m r s

evalAnalyzer
    :: Analyzer exprMetadata astMetadata a
    -> AnalyzerReader
    -> AnalyzerState exprMetadata astMetadata
    -> (These AnalyzerWriter a)
evalAnalyzer m r s = runIdentity $ evalAnalyzerT m r s

execAnalyzer
    :: Analyzer exprMetadata astMetadata a
    -> AnalyzerReader
    -> AnalyzerState exprMetadata astMetadata
    -> AnalyzerState exprMetadata astMetadata
execAnalyzer m r s = runIdentity $ execAnalyzerT m r s

runAnalyzerT
    :: AnalyzerT exprMetadata astMetadata m a
    -> AnalyzerReader
    -> AnalyzerState exprMetadata astMetadata
    -> m (These AnalyzerWriter a, AnalyzerState exprMetadata astMetadata)
runAnalyzerT m r s = flip runStateT s $ flip runReaderT r $ runChronicleT m

evalAnalyzerT
    :: (Monad m)
    => AnalyzerT exprMetadata astMetadata m a
    -> AnalyzerReader
    -> AnalyzerState exprMetadata astMetadata
    -> m (These AnalyzerWriter a)
evalAnalyzerT m r s = flip evalStateT s $ flip runReaderT r $ runChronicleT m

execAnalyzerT
    :: (Monad m)
    => AnalyzerT exprMetadata astMetadata m a
    -> AnalyzerReader
    -> AnalyzerState exprMetadata astMetadata
    -> m (AnalyzerState exprMetadata astMetadata)
execAnalyzerT m r s = flip execStateT s $ flip runReaderT r $ runChronicleT m

withAnalyzerT
    :: (AnalyzerReader -> AnalyzerReader)
    -> (AnalyzerState exprMetadata astMetadata -> AnalyzerState exprMetadata astMetadata)
    -> AnalyzerT exprMetadata astMetadata m a
    -> AnalyzerT exprMetadata astMetadata m a
withAnalyzerT fr fs at =
    ChronicleT let rt = runChronicleT at in
        ReaderT \r -> let st = runReaderT rt (fr r) in
            StateT \s -> runStateT st (fs s)

withScope :: (Monad m) => AnalyzerT exprMetadata astMetadata m a -> AnalyzerT exprMetadata astMetadata m a
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

currentModuleName :: (Monad m) => AnalyzerT exprMetadata astMetadata m Name
currentModuleName = gets (moduleName . currentModule)

newError :: Name -> Error -> AnalyzerWriter
newError moduleName e = mempty { errors = Map.singleton moduleName [e] }

addError :: (Monad m) => Error -> AnalyzerT exprMetadata astMetadata m ()
addError e = currentModuleName >>= \name -> dictate $ newError name e

fatalError :: (Monad m) => Error -> AnalyzerT exprMetadata astMetadata m a
fatalError e = currentModuleName >>= \name -> confess $ newError name e

newWarning :: Name -> Warning -> AnalyzerWriter
newWarning moduleName w = mempty { warnings = Map.singleton moduleName [w] }

addWarning :: (Monad m) => Warning -> AnalyzerT exprMetadata astMetadata m ()
addWarning w = currentModuleName >>= \name -> dictate $ newWarning name w

addVar :: Name -> Type -> Analyzer e' a' VariableInfo
addVar n t = do
    symbols <- gets analyzerSymbols
    let info = mkInfo n t
    if Map.member n symbols
        then addError (ShadowedVariable n)
        else modify \s -> s { analyzerSymbols = Map.insert n info symbols }
    pure info

addVars :: (Traversable t) => t (Name, Type) -> Analyzer e' a' (t VariableInfo)
addVars = Traversable.traverse (uncurry addVar)

analyzeAssign
    :: Expression e
    -> Expression e
    -> Analyzer e' a' (Expression Type, Expression Type)
analyzeAssign left right = do
    exprL <- analyzeExpr left
    let typeL = getMetadata exprL
    exprR <- analyzeExprWithHint typeL right
    let typeR = getMetadata exprR
    when (typeL /= typeR) $
        addError $ TypeMismatch (unsafeCodegen' left <> " = " <> unsafeCodegen' right) typeL typeR
    pure (exprL, exprR)

analyzeVar :: Name -> Type -> Expression e -> Analyzer e' a' (Expression Type)
analyzeVar name type' expr = do
    exprR <- analyzeExprWithHint type' expr
    checkTypes name type' (getMetadata exprR)
    addVar name type'
    pure exprR

checkTypes :: Text -> Type -> Type -> Analyzer e' a' ()
checkTypes name expectedType actualType
    | expectedType == actualType = pure ()
    | otherwise = addError $ TypeMismatch name expectedType actualType

-- TODO (Optional): Check for unused modules.
-- TODO: Maybe rewrite this algorithm to remove dependency on algebraic-graphs?
linkModules :: [Module e a] -> Analyzer e' a' (Map Name (ModuleImports e a))
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

collect :: Module e a -> Analyzer e' a' (Map Name VariableInfo)
collect mod' = do
    exts' <- addVars exts
    funcs' <- addVars funcs
    adts <- algebraicToFunctions (adtTemplates mod')
    pure $ Map.unions [exts', funcs', adts]
  where
    exts = Map.mapWithKey (,) (externs mod')
    funcs = Map.fromList $ getFuncNameType <$> functions mod'
    getFuncNameType (Function _ n t _ _) = (n, (n, t))

algebraicToFunctions :: Map Name [Constructor Type] -> Analyzer e' a' (Map Name VariableInfo)
algebraicToFunctions = fmap Map.unions . Traversable.traverse (addVars . Map.mapWithKey (,)) . Map.elems . Map.mapWithKey fromConstructors
  where
    fromConstructors adtName constructors =
        Map.fromList (map (fromConstructor adtName) constructors)

    fromConstructor adtName (Constructor cName cType) = case cType of
        Nothing     -> (cName, AlgebraicType adtName)
        Just cType' -> (cName, FunctionType [cType'] (AlgebraicType adtName))

analyzeImpl :: Map Name (ModuleImports e a) -> Analyzer e a (Map Name (ModuleImports Type a))
analyzeImpl mods = forM mods \(ModuleImports _ imports root) -> do
    analyzed <- gets modulesAnalyzedSoFar
    case Map.lookup (moduleName root) analyzed of
        Just root' -> pure root'
        Nothing -> do
            mods' <- analyzeImpl imports
            let cache = Map.unions $ map cachedSymbols $ Map.elems mods'
            modify \s -> s
                { analyzerSymbols = cache
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


analyze :: NonEmpty (Module e a) -> These AnalyzerWriter (Map Name (Module Type a))
analyze (root :| mods) = fst $ runAnalyzer (analyze' (root : mods)) def def

analyze' :: [Module e a] -> Analyzer e a (Map Name (Module Type a))
analyze' mods = fmap getMods . analyzeImpl =<< linkModules mods
  where
    getMods = Map.foldr go Map.empty
    go (ModuleImports _ ms m) acc = Map.insert (moduleName m) m (Map.union acc (getMods ms))

analyzeFunction :: Function e a -> Analyzer e' a' (Function Type a)
analyzeFunction (Function _ name (FunctionType argTypes ret) arguments next) = do
    let nArgs = length arguments
        nTypes = length argTypes
    when (nArgs /= nTypes) $
        addError $ IncompatibleSignatures name nArgs nTypes
    ast <- withScope do
        void (addVars (zip arguments argTypes))
        withAnalyzerT id (\s -> s { returnType = ret }) (analyzeAst next)
    pure $ Function (getMetadata ast) name (FunctionType argTypes ret) arguments ast
analyzeFunction (Function _ name _ _ _) = fatalError (NotAFunction name)

analyzeReturn :: Maybe (Expression e) -> Analyzer e' a' (Expression Type)
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

findAdt :: Name -> Analyzer e' a' (Maybe (Name, [Constructor Type], Constructor Type))
findAdt cName = do
    adts <- gets (adtTemplates . currentModule)
    pure $ findMap go $ Map.toList adts
  where
    go (adtName, adtConstructors) =
        (adtName, adtConstructors,) <$> find ((== cName) . constructorName) adtConstructors

analyzePattern' :: MatchPattern -> Analyzer e' a' ()
analyzePattern' = \case
    AlgebraicPattern (Constructor cName field) -> findAdt cName >>= \case
        Just (_adtName, _constructors, Constructor _cName cType) -> case (field, cType) of
            (Just field', Just cType') -> void (analyzePattern cType' field')
            (Just field', Nothing    ) -> analyzePattern' field'
            _                          -> pure ()
        Nothing -> addError (NoSuchConstructor cName)
    ArrayPattern positions -> traverse_ analyzePattern' positions
    LiteralPattern (Double d) -> addWarning (FloatingPointEquality d)
    LiteralPattern _ -> pure ()
    NamePattern _ -> pure ()
    RecordPattern fields -> do
        void (analyzeDuplicates fields)
        traverse_ (analyzePattern' . fieldValue) fields

analyzePattern :: Type -> MatchPattern -> Analyzer e' a' (Map Name VariableInfo)
analyzePattern expectedType = \case
    AlgebraicPattern (Constructor cName field) -> findAdt cName >>= \case
        Just (adtName, _constructors, Constructor _cName cType)
            | AlgebraicType adtName == expectedType -> case (field, cType) of
                (Just field', Just cType') -> analyzePattern cType' field'
                _                          -> pure Map.empty
            | otherwise -> mismatch cName (AlgebraicType adtName)
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
    NamePattern name -> pure (Map.singleton name (mkInfo name expectedType))
    RecordPattern fields -> do
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
                let expr = patternToExpr $ RecordPattern sorted
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
    -> [(MatchPattern, AST e a)]
    -> Analyzer e' a' (Expression Type, [(MatchPattern, AST Type a)])
analyzeMatch expr branches = do
    exprWithMetadata <- analyzeExpr expr
    branches' <- traverse (go exprWithMetadata []) branches
    pure (exprWithMetadata, branches')
  where
    go e _patterns (p, a) = withScope do
        infos <- analyzePattern (getMetadata e) p
        modify \s -> s { analyzerSymbols = Map.union infos (analyzerSymbols s) }
        ast' <- analyzeAst a
        pure (p, ast')

analyzeAst :: AST e a -> Analyzer e' a' (AST Type a)
analyzeAst = \case
    Assign m left right next -> do
        (left', right') <- analyzeAssign left right
        Assign m left' right' <$> analyzeAst next
    End m -> pure $ End m
    Expression m expr next -> Expression m <$> analyzeExpr expr <*> analyzeAst next
    If m cond true false next -> do
        cond' <- analyzeExprWithHint boolType cond
        checkTypes "if" boolType (getMetadata cond')
        true'  <- withScope $ analyzeAst true
        false' <- withScope $ analyzeAst false
        If m cond' true' false' <$> analyzeAst next
    Match m expr branches next -> do
        (expr', branches') <- analyzeMatch expr branches
        Match m expr' branches' <$> analyzeAst next
    Return m exprMaybe -> Return m <$> (Just <$> analyzeReturn exprMaybe)
    Var m var type' expr next -> do
        Var m var type' <$> analyzeVar var type' expr <*> analyzeAst next
    While m cond body next -> do
        cond' <- analyzeExprWithHint boolType cond
        checkTypes "while" boolType (getMetadata cond')
        body' <- withScope $ analyzeAst body
        While m cond' body' <$> analyzeAst next

analyzeExpr :: Expression e -> Analyzer e' a' (Expression Type)
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
        s <- analyzeStructure struct
        pure $ Structure (getMetadata s) s
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

analyzeExprWithHint :: Type -> Expression e -> Analyzer e' a' (Expression Type)
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
        s <- analyzeStructureWithHint expectedType struct
        let typeS = getMetadata s
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

analyzeApply :: Name -> [Type] -> Type -> Analyzer e' a' Type
analyzeApply name arguments (FunctionType argTypes ret) = do
    let nArgs = length arguments
        nTypes = length argTypes
    when (nArgs /= nTypes) $ addError $ IncompatibleSignatures name nArgs nTypes
    traverse_ (uncurry (checkTypes name)) $ zip argTypes arguments
    pure ret
analyzeApply name _ type' = addError (NotAFunction name) *> pure type'

markUsed :: VariableInfo -> VariableInfo
markUsed info = info { unused = False }

analyzeVariable :: Name -> Analyzer e' a' VariableInfo
analyzeVariable name = do
    (infoMaybe, symbols) <- gets $
        Map.updateLookupWithKey (const (Just . markUsed)) name . analyzerSymbols
    modify \s -> s { analyzerSymbols = symbols }
    maybe (fatalError (UndefinedVariable name)) pure infoMaybe

analyzeArray :: [Expression e] -> Analyzer e' a' (Structure Type)
analyzeArray [] = fatalError UnknownArray
analyzeArray (x : xs) = do
    xExpr <- analyzeExpr x
    let xType = getMetadata xExpr
    xExprs <- traverse (analyzeExprWithHint xType) xs
    pure $ Array xType (xExpr : xExprs)

-- FIXME: Still not properly typechecking!
analyzeArrayWithHint :: Type -> [Expression e] -> Analyzer e' a' (Structure Type)
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

unzipFields :: [Field e] -> ([Name], [e])
unzipFields = unzip . map (\(Field n v) -> (n, v))

-- TODO: Add types to error?
analyzeDuplicates :: [Field e] -> Analyzer e' a' [Field e]
analyzeDuplicates fields = traverse_ (addError . DuplicateRecord . fst) dups *> pure sorted
  where
    sorted = sortOn fieldName fields
    sortedNames = map fieldName sorted
    dups = filter (uncurry (==)) $ zip sortedNames (drop 1 sortedNames)

analyzeRecord :: [Field (Expression e)] -> Analyzer e' a' (Structure Type)
analyzeRecord fields = do
    sorted <- analyzeDuplicates fields
    let (names, exprs) = unzipFields sorted

    exprsWithMetadata <- traverse analyzeExpr exprs
    let types = getMetadata <$> exprsWithMetadata
    pure $ Record (RecordType $ zipWith Field names types) (zipWith Field names exprsWithMetadata)

analyzeRecordWithHint :: [Field Type] -> [Field (Expression e)] -> Analyzer e' a' (Structure Type)
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
        addError $ TypeMismatch (unsafeCodegen' $ Record () (void <<$>> sorted)) (RecordType expectedSorted) (RecordType actualFields)

    pure $ Record (RecordType actualFields) (zipWith Field names exprsWithMetadata)

-- TODO: analyzeAlgebraicWithHint
analyzeAlgebraic :: Constructor (Expression e) -> Analyzer e' a' (Structure Type)
analyzeAlgebraic (Constructor name expr) = findAdt name >>= \case
    Nothing -> do
        whenJust expr (void . analyzeExpr)
        fatalError $ NoSuchConstructor name
    Just (adtName, _adtConstructors, adtConstructor) -> do
        field <- sequence $ liftA2 analyzeExprWithHint (constructorValue adtConstructor) expr
        pure $ Algebraic (AlgebraicType adtName) (Constructor name field)

literalType :: Literal -> Type
literalType = \case
    Char _    -> CharType
    Double _  -> DoubleType
    Integer _ -> IntegerType
    Text _    -> TextType

analyzeStructure :: Structure e -> Analyzer e' a' (Structure Type)
analyzeStructure = \case
    Algebraic _ constructor -> analyzeAlgebraic constructor
    Array _ elements        -> analyzeArray elements
    Record _ fields         -> analyzeRecord fields

analyzeStructureWithHint :: Type -> Structure e -> Analyzer e' a' (Structure Type)
analyzeStructureWithHint expectedType = \case
    Algebraic _ constructor -> analyzeAlgebraic constructor
    Array _ elements        -> analyzeArrayWithHint expectedType elements
    Record _ fields         -> case expectedType of
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
