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
    , AnalyzedModule (..)
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
    , includeNumber :: !Int
    } deriving (Show)

data AnalyzerState exprMetadata = AnalyzerState
    { analyzerSymbols      :: !(Map Name VariableInfo)
    , currentModule        :: !(Module exprMetadata)
    , adtsInScope          :: !(Map Name [Constructor Type])
    , modulesAnalyzedSoFar :: !(Map Name (ModuleImports Type))
    , functionName         :: !Name
    , returnType           :: !Type
    , foundAcceptableMain  :: !Bool
    } deriving (Show)

instance Default (AnalyzerState exprMetadata) where
    def = AnalyzerState Map.empty (mkModule "") Map.empty Map.empty "" unitType False

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
    checkTypes (unsafeCodegen' left <> " = " <> unsafeCodegen' right) typeL typeR
    pure (exprL, exprR)

analyzeVar :: Name -> Type -> Expression e -> Analyzer e' (Expression Type)
analyzeVar name type' expr = do
    exprR <- analyzeExprWithHint type' expr
    checkTypes name type' (getMetadata exprR)
    addVar name type'
    pure exprR

checkTypes :: Text -> Type -> Type -> Analyzer e' ()
checkTypes name expectedType actualType
    | expectedType == actualType = pass
    | otherwise = addError $ TypeMismatch name expectedType actualType

-- TODO (Optional): Check for unused modules.
-- TODO: Maybe rewrite this algorithm to remove dependency on algebraic-graphs?
linkModules :: forall e e'. [Module e] -> Analyzer e' (Map Name (ModuleImports e))
linkModules mods = do
    traverse_ (addError . DuplicateModule . moduleName) repeated
    case AM.Algorithm.topSort graph of
        Left cyclicImports -> fatalError $ CyclicImports cyclicImports
        Right order -> forest $ sortByOrder order
  where
    (repeated, adj) = first catMaybes $ foldr go ([], Map.empty) mods
      where
        go m (repeatedMods, acc) =
            first (: repeatedMods) $ Map.insertLookupWithKey (\_ n _ -> n) (moduleName m) m acc

    graph = AM.fromAdjacencySets $ map (moduleName &&& Set.fromList . importedModules) mods

    sortByOrder :: [Name] -> Map Name (Int, Module e)
    sortByOrder order = Map.fromList $ zipWith (\i n -> (n, (i, adj Map.! n))) [0..] (reverse order)

    forest :: Map Name (Int, Module e) -> Analyzer e' (Map Name (ModuleImports e))
    forest msTotal = forestImpl msTotal
      where
        forestImpl :: Map Name (Int, Module e) -> Analyzer e' (Map Name (ModuleImports e))
        forestImpl ms = do
            trees <- traverse tree ms
            pure $ fmap snd trees

        tree :: (Int, Module e) -> Analyzer e' (Name, ModuleImports e)
        tree (i, m) = do
            traverse_ (addError . NoSuchModule) missing
            imports <- forestImpl $ Map.fromList $ map (\n -> (n, msTotal Map.! n)) $ map moduleName modules
            pure (moduleName m, ModuleImports Map.empty imports m i)
          where
            (missing, modules) = partitionEithers moduleLookups

            moduleLookups :: [Either Name (Module e)]
            moduleLookups = map lookupModule (importedModules m)

            lookupModule :: Name -> Either Name (Module e)
            lookupModule name = maybe (Left name) Right (Map.lookup name adj)

collect :: Module e -> Analyzer e' (Map Name VariableInfo)
collect mod' = liftA2 Map.union (addVars exts) (addVars funcs)
  where
    exts = Map.mapWithKey (,) (externs mod')
    funcs = Map.fromList $ getFuncNameType <$> functions mod'
    getFuncNameType (Function n t _ _) = (n, (n, t))

analyzeImpl :: Map Name (ModuleImports e) -> Analyzer e (Map Name (ModuleImports Type))
analyzeImpl mods = forM mods \(ModuleImports _ imports root i) -> do
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
                    , includeNumber = i
                    }
            modify \s -> s
                { modulesAnalyzedSoFar = Map.insert (moduleName root) mi (modulesAnalyzedSoFar s)
                }
            pure mi

data AnalyzedModule = AnalyzedModule
    { amRootModule    :: !(Module Type)
    , amIncludeNumber :: !Int
    } deriving (Show)

analyze :: [Module e] -> These AnalyzerWriter (Map Name AnalyzedModule)
analyze mods = nonFatalToFatal $ fst $ runAnalyzer (analyze' mods) def def
  where
    nonFatalToFatal = \case
        These writer@(AnalyzerWriter es ws) mod'
            | null es &&      null ws  -> That mod'
            | null es && not (null ws) -> These writer mod'
            | otherwise                -> This writer
        other -> other

analyze' :: [Module e] -> Analyzer e (Map Name AnalyzedModule)
analyze' mods = do
    analysis <- analyzeImpl =<< linkModules mods
    modify \st -> st { currentModule = mkModule "" }
    foundMain <- gets foundAcceptableMain
    if foundMain
        then pass
        else addError MainNotFound
    pure $ getMods analysis
  where
    getMods = Map.foldr go Map.empty
    go (ModuleImports _ ms m i) acc =
        Map.insert (moduleName m) (AnalyzedModule m i) (Map.union acc (getMods ms))

analyzeFunction :: Function e -> Analyzer e' (Function Type)
analyzeFunction f@(Function name (FunctionType argTypes ret) arguments body) = do
    when (isMainFunction f) $
        modify \st -> st { foundAcceptableMain = True }

    zipped <- zipExact name arguments argTypes
    asts <- withScope do
        void (addVars zipped)
        withAnalyzerT id (\s -> s { returnType = ret, functionName = name }) (analyzeAstsImpl body)
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

--findCtor :: Name -> Analyzer e' [Constructor Type]
--findCtor cName = do
--    adts <- gets adtsInScope
--    pure $ concat $ filter (any ((== cName) . constructorName)) $ Map.elems adts

--findAdt :: Name -> [Constructor Type] -> Maybe (Constructor Type)
--findAdt adtName = find ((== adtName) . constructorAdt)

findAdtByNameCtor :: Name -> Name -> Analyzer e' (Maybe (Constructor Type))
findAdtByNameCtor adt ctor = do
    adts <- gets adtsInScope
    pure $ findMap (find (\a -> (constructorName a == ctor) && (constructorAdt a == adt))) $ Map.elems adts

analyzePattern :: Type -> MatchPattern -> Analyzer e' (Map Name (Name, Type))
analyzePattern expectedType = \case
    DiscardPattern _ -> pure Map.empty
    LiteralPattern lit
        | literalType lit == expectedType -> do
            when (expectedType == DoubleType) $
                addWarning (FloatingPointEquality (getDouble lit))
            pure Map.empty
        | otherwise -> mismatch (unsafeCodegen' lit) (literalType lit)
    NamePattern name -> pure (Map.singleton name (name, expectedType))
    StructurePattern struct -> case struct of
        Algebraic (Constructor adtName cName field) -> findAdtByNameCtor adtName cName >>= \case
            Just (Constructor _adtName cName' cType)
                | AlgebraicType adtName == expectedType -> do
                    when (cName /= cName') $
                        addError (ConstructorMismatch adtName cName' cName)
                    case (field, cType) of
                        (Just field', Just cType') -> analyzePattern cType' field'
                        _                          -> pure Map.empty
                | otherwise -> mismatch cName (AlgebraicType adtName)
            Nothing -> addError (NoSuchConstructor adtName cName) $> Map.empty
        Array positions -> case expectedType of
            ArrayType t -> Map.unions <$> traverse (analyzePattern t) positions
            _           -> addError (UnknownPattern (StructurePattern $ Array positions) expectedType) $> Map.empty
        Record fields -> do
            sorted <- analyzeDuplicates fields
            case expectedType of
                RecordType fields' -> do
                    sorted' <- analyzeDuplicates fields'
                    zipped <-
                        zipWithExact
                            (unsafeCodegen' $ Record fields)
                            analyzePattern
                            (map fieldValue sorted')
                            (map fieldValue sorted)
                    let names = Set.fromList (map fieldName sorted)
                        names' = Set.fromList (map fieldName sorted')
                        -- TODO: Review whether this is how missing and extra should really be done.
                        missing = names' Set.\\ names
                        extra = names Set.\\ names'
                    traverse_ (addError . UndefinedVariable) extra
                    whenJust (nonEmpty (Set.elems missing)) (addError . MissingFields)
                    Map.unions <$> sequence zipped
                _ -> addError (UnknownPattern (StructurePattern $ Record fields) expectedType) $> Map.empty
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
        void $ addVars infos
        Branch p <$> analyzeAsts as

findUnreachable :: [AST e] -> (Bool, [AST e])
findUnreachable = second (concat . maybeToList) . go
  where
    go :: [AST e] -> (Bool, Maybe [AST e])
    go = \case
        If _ true false : xs -> next xs $ go true `combineOr` go false
        Return _ : [] -> (False, Nothing)
        Return _ : x : _ -> (False, Just [x])
        While _ body : xs -> next xs $ go body
        Match _ branches : xs -> next xs $ combineMany $ map (\(Branch _ branch) -> go branch) branches
        _ : xs -> go xs
        [] -> (True, Nothing)
      where
        next []       y = y
        next (x : xs) y = y `combineAnd` if fst y then go (x : xs) else (True, Just [x])

    combineAnd (b, a) (b', a') = (b && b', a <> a')
    combineOr  (b, a) (b', a') = (b || b', a <> a')

    combineMany [] = (False, Nothing)
    combineMany (x : xs) = x `combineOr` combineMany xs

analyzeAsts :: [AST e] -> Analyzer e' [AST Type]
analyzeAsts = fmap catMaybes . traverse analyzeAstNonFatal
  where
    analyzeAstNonFatal ast = memento (analyzeAst ast) >>=
        either
            (\writer -> do
                traverse_ addError   (concatFromMap $ errors   writer)
                traverse_ addWarning (concatFromMap $ warnings writer)
                pure Nothing)
            (pure . Just)

    concatFromMap = concat . Map.elems

analyzeAstsImpl :: [AST e] -> Analyzer e' [AST Type]
analyzeAstsImpl asts = do
    let (isReachable, unreachableAsts) = findUnreachable asts
    st <- get
    when (isReachable && returnType st /= unitType) $
        addError $ TypeMismatch (functionName st) (returnType st) unitType
    traverse_ (addWarning . UnreachableStatement . void) unreachableAsts

    -- Explicitly add a return at the end if none exists:
    if isReachable && returnType st == unitType
        then analyzeAsts asts <&> (<> [Return $ Just $ Structure unitType unit'])
        else analyzeAsts asts

analyzeAst :: AST e -> Analyzer e' (AST Type)
analyzeAst = \case
    Assign left right -> uncurry Assign <$> analyzeAssign left right
    Expression expr -> Expression <$> analyzeExpr expr
    If cond true false -> liftA3 If (analyzeExprWithHint boolType cond) (withScope $ analyzeAsts true) (withScope $ analyzeAsts false)
    Match expr branches -> uncurry Match <$> analyzeMatch expr branches
    Return exprMaybe -> Return <$> (Just <$> analyzeReturn exprMaybe)
    Var var type' expr -> Var var type' <$> analyzeVar var type' expr
    While cond body -> liftA2 While (analyzeExprWithHint boolType cond) (withScope $ analyzeAsts body)

indexType :: Type -> Maybe Type
indexType = \case
    ArrayType t -> Just t
    TextType    -> Just CharType
    _           -> Nothing

-- HACK: access{Array,Text} are workarounds until we have generics.
accessArray :: Type -> Expression Type -> Name -> Analyzer e' (Expression Type)
accessArray elementType left right =
    Map.lookup right leftRight & maybe
        (fatalError (NotAMember arrayType right))
        (\t -> pure $ Access t left right)
  where
    leftRight = Map.fromList
        [ ("push", addFuncType)
        , ("concat", appendFunctionType)
        , ("length", lengthType)
        ]

    arrayType = getMetadata left

    addFuncType        = FunctionType [elementType] arrayType
    appendFunctionType = FunctionType [arrayType] arrayType
    lengthType         = IntegerType

accessText :: Expression Type -> Name -> Analyzer e' (Expression Type)
accessText left right =
    Map.lookup right leftRight & maybe
        (fatalError (NotAMember TextType right))
        (\t -> pure $ Access t left right)
  where
    leftRight = Map.fromList
        [ ("push", addFuncType)
        , ("concat", appendFunctionType)
        , ("length", lengthType)
        ]

    addFuncType        = FunctionType [CharType] TextType
    appendFunctionType = FunctionType [TextType] TextType
    lengthType         = IntegerType

analyzeExpr :: Expression e -> Analyzer e' (Expression Type)
analyzeExpr = \case
    Access _ left right -> do
        exprL <- analyzeExpr left
        let typeL = getMetadata exprL
        case typeL of
            RecordType fieldTypes -> case find (\(Field n _) -> n == right) fieldTypes of
                Nothing -> fatalError (NotAMember typeL right)
                Just (Field _ fieldType) -> pure (Access fieldType exprL right)
            ArrayType elementType -> accessArray elementType exprL right
            TextType -> accessText exprL right
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
    Call _ expr arguments -> do
        exprE <- analyzeExpr expr
        let typeE = getMetadata exprE
            genE = unsafeCodegen' expr
        case typeE of
            FunctionType argTypes _ -> do
                zipped <- zipWithExact
                    (unsafeCodegen' $ Call () (void expr) (map void arguments))
                    analyzeExprWithHint
                    argTypes
                    arguments
                exprAs <- sequence zipped
                let typeAs = getMetadata <$> exprAs
                ret <- analyzeApply genE typeAs typeE
                pure $ Call ret exprE exprAs
            _ -> fatalError (NotAFunction genE)
    Index _ left right -> do
        exprL <- case left of
            -- TODO: Change unitType with a generic type once it's implemented.
            Structure _ (Array _) -> analyzeExprWithHint (ArrayType unitType) left
            Literal _ (Text _) -> analyzeExprWithHint TextType left
            _ -> analyzeExpr left
        let typeL = getMetadata exprL
        indexed <- maybe
            (addError (TypeMismatch (unsafeCodegen' left) (ArrayType unitType) typeL) $> typeL)
            pure
            (indexType typeL)
        exprR <- analyzeExprWithHint IntegerType right
        pure $ setMetadata indexed $ Index typeL exprL exprR
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
                Nothing -> addError (NotAMember typeL right) $> Access expectedType exprL right
                Just (Field _ typeR) -> do
                    unless (typeR == expectedType) $
                        addError $ TypeMismatch (unsafeCodegen' (Access () (void left) right)) expectedType typeR
                    pure $ Access expectedType exprL right
            ArrayType elementType -> accessArray elementType exprL right
            TextType -> accessText exprL right
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
                pure $ BinaryOp expectedType exprL symbol' exprR
            Just _ -> pure $ BinaryOp expectedType exprL symbol' exprR
    Call _ expr arguments -> do
        exprE <- analyzeExpr expr
        let typeE = getMetadata exprE
            genE = unsafeCodegen' expr
        case typeE of
            FunctionType argTypes _ -> do
                zipped <- zipWithExact
                    (unsafeCodegen' $ Call () (void expr) (map void arguments))
                    analyzeExprWithHint
                    argTypes
                    arguments
                exprAs <- sequence zipped
                let typeAs = getMetadata <$> exprAs
                void $ analyzeApply genE typeAs typeE
                pure $ Call expectedType exprE exprAs
            _ -> do
                addError (NotAFunction genE)
                exprAs <- traverse analyzeExpr arguments
                pure $ Call expectedType exprE exprAs
    Index _ left right -> do
        exprL <- case left of
            Structure _ (Array _) -> analyzeExprWithHint (ArrayType expectedType) left
            Literal _ (Text _) -> analyzeExprWithHint TextType left
            _ -> analyzeExpr left
        let typeL = getMetadata exprL
        whenNothing_ (indexType typeL) $
            fatalError (TypeMismatch (unsafeCodegen' left) (ArrayType expectedType) typeL)
        exprR <- analyzeExprWithHint IntegerType right
        pure $ Index expectedType exprL exprR
    Literal _ l -> do
        let typeL = literalType l
        when (typeL /= expectedType) $
            addError (TypeMismatch (unsafeCodegen' $ Literal () l) expectedType typeL)
        pure $ Literal expectedType l
    Parenthesis _ expr -> analyzeExprWithHint expectedType expr
    Structure _ struct -> do
        (_, s) <- analyzeStructureWithHint expectedType struct
        --when (typeS /= expectedType) $
        --    addError (TypeMismatch (unsafeCodegen' struct) expectedType typeS)
        pure $ Structure expectedType s
    UnaryOp _ symbol' expr -> do
        exprE <- analyzeExprWithHint expectedType expr
        let typeE = getMetadata exprE
        case interactsWithUnary symbol' typeE of
            Nothing -> do
                addError (IncompatibleTypes1 symbol' (void expr))
                pure $ UnaryOp expectedType symbol' exprE
            Just _ -> pure $ UnaryOp expectedType symbol' exprE
    Variable _ v -> do
        typeV <- varType <$> analyzeVariable v
        when (typeV /= expectedType) $
            addError (TypeMismatch v expectedType typeV)
        pure $ Variable expectedType v

analyzeApply :: Name -> [Type] -> Type -> Analyzer e' Type
analyzeApply name arguments (FunctionType argTypes ret) = do
    zipped <- zipExact name arguments argTypes
    traverse_ (uncurry (checkTypes name)) zipped
    pure ret
analyzeApply name _ type' = addError (NotAFunction name) $> type'

zipExact :: Name -> [a] -> [b] -> Analyzer e' [(a, b)]
zipExact n = zipWithExact n (,)

zipWithExact :: Name -> (a -> b -> c) -> [a] -> [b] -> Analyzer e' [c]
zipWithExact n f as bs = go as bs
  where
    go [] [] = pure []
    go (x : xs) (y : ys) = (f x y :) <$> go xs ys
    go _ _ = addError (IncompatibleSignatures n (length as) (length bs)) $> []

markUsed :: VariableInfo -> VariableInfo
markUsed info = info { unused = False }

analyzeVariable :: Name -> Analyzer e' VariableInfo
analyzeVariable name = do
    (infoMaybe, symbols) <- gets $
        Map.updateLookupWithKey (const (Just . markUsed)) name . analyzerSymbols
    modify \s -> s { analyzerSymbols = symbols }
    maybe (fatalError (UndefinedVariable name)) pure infoMaybe

analyzeArray :: [Expression e] -> Analyzer e' (Type, Structure (Expression Type))
analyzeArray [] = fatalError (UnknownType $ Structure () $ Array [])
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
analyzeDuplicates fields = traverse_ (addError . DuplicateRecord . fst) dups $> sorted
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
    exprsWithMetadata <- zipWithM analyzeExprWithHint expectedTypes exprs
    let types = getMetadata <$> exprsWithMetadata
        actualFields = zipWith Field names types
    when (expectedSorted /= actualFields) $
        addError $ TypeMismatch (unsafeCodegen' $ Record (void <<$>> sorted)) (RecordType expectedSorted) (RecordType actualFields)

    pure (RecordType actualFields, Record (zipWith Field names exprsWithMetadata))

-- TODO: analyzeAlgebraicWithHint
-- TODO: check if constructor is contained inside another ADT inside
analyzeAlgebraic :: Constructor (Expression e) -> Analyzer e' (Type, Structure (Expression Type))
analyzeAlgebraic (Constructor adtName name expr) = findAdtByNameCtor adtName name >>= \case
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
