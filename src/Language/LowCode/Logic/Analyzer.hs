module Language.LowCode.Logic.Analyzer
    ( AnalyzerState (..)
    , LogicAnalyzerT
    , emptyState
    , execAnalyzerT
    , analyze
    , analyzeExpr
    ) where

import Universum

import           Control.Monad.Trans.Except (Except, throwE)
import qualified Data.Map.Strict as Map

import           Language.Common
import           Language.LowCode.Logic.AST
import           Language.LowCode.Logic.Prim
import qualified Language.LowCode.Logic.Types as L

-- TODO: Should IncompatibleTypes and TypeMismatch really have these signatures?
-- Or even better: Should TypeMismatch instead have info about the node instead
-- of simply a Text?
data Error
    = IncompatibleSignatures Name Int Int
    | IncompatibleTypes1 UnarySymbol Expression
    | IncompatibleTypes2 Expression BinarySymbol Expression
    | NotAFunction Name
    | StartIsNotFirstSymbol
    | ShadowedVariable Name
    | TypeMismatch Text L.VariableType L.VariableType
    | UndefinedVariable Name
    deriving (Eq, Show)

data Warning
    = UnusedVariable Text
    deriving (Eq, Show)

data VariableInfo = VariableInfo
    { varName :: Name
    , unused  :: Bool
    , varType :: L.VariableType
    } deriving (Show)

mkInfo :: Name -> L.VariableType -> VariableInfo
mkInfo name type' = VariableInfo name True type'

data AnalyzerState = AnalyzerState
    { analyzerSymbols :: Map Text VariableInfo
    , errors          :: [Error]
    , warnings        :: [Warning]
    } deriving (Show)

-- It should be explained why there is an error accumulator in the state, as
-- well as an error in the transformer itself: some errors don't prevent the
-- analyzer from checking for more problems (like an undefined variable), but
-- some must stop (and possibly be caught), because the error means that the
-- entire expression is malformed. For example, in case a variable is undefined
-- in an Expression, or it doesn't typecheck.
type LogicAnalyzerT = StateT AnalyzerState (Except Error)

emptyState :: AnalyzerState
emptyState = AnalyzerState Map.empty [] []

execAnalyzerT :: AnalyzerState -> LogicAnalyzerT a -> Either Error AnalyzerState
execAnalyzerT st = runIdentity . runExceptT . flip execStateT st

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

analyzeVar :: Text -> L.VariableType -> Expression -> LogicAnalyzerT ()
analyzeVar var type' expr = do
    symbols <- gets analyzerSymbols
    if Map.member var symbols
    then addError $ ShadowedVariable var
    else do
        analyzeExprTypes var type' expr
        let info = mkInfo var type'
        modify \st -> st { analyzerSymbols = Map.insert var info symbols }

checkTypes :: Text -> L.VariableType -> L.VariableType -> LogicAnalyzerT ()
checkTypes name expectedType actualType
    | expectedType == actualType = pure ()
    | otherwise = addError $ TypeMismatch name expectedType actualType

analyzeExprTypes :: Text -> L.VariableType -> Expression -> LogicAnalyzerT ()
analyzeExprTypes exprName expectedType expr =
    whenJustM (analyzeExpr expr) (checkTypes exprName expectedType)

-- In case Call has a name instead of expr:
--checkApply :: Name -> L.VariableType -> [Expression] -> LogicAnalyzerT (Maybe L.VariableType)
--checkApply name (L.FunctionType argTypes ret) arguments = do
--    let nArgs = length arguments
--        nTypes = length argTypes
--    when (nArgs /= nTypes) $
--        addError $ IncompatibleSignatures name nArgs nTypes
--    traverse_ (uncurry (analyzeExprTypes name)) $ zip argTypes arguments
--    pure $ Just ret
--checkApply name _ _ = addError (NotAFunction name) *> pure Nothing

-- TODO: Generate more descriptive names!
checkApply :: L.VariableType -> [Expression] -> LogicAnalyzerT (Maybe L.VariableType)
checkApply (L.FunctionType argTypes ret) arguments = do
    let nArgs = length arguments
        nTypes = length argTypes
    when (nArgs /= nTypes) $
        addError $ IncompatibleSignatures "function" nArgs nTypes
    traverse_ (uncurry (analyzeExprTypes "function")) $ zip argTypes arguments
    pure $ Just ret
checkApply _ _ = addError (NotAFunction "not function lol") *> pure Nothing

analyze :: L.Metadata -> AST -> LogicAnalyzerT ()
analyze metadata (Start name (L.FunctionType argTypes ret) arguments next) = do
    let nArgs = length arguments
        nTypes = length argTypes
    when (nArgs /= nTypes) $
        addError $ IncompatibleSignatures name nArgs nTypes
    mkSymbols
    whenJustM (withScope $ analyzeImpl next) $ checkTypes name ret
  where
    mkSymbols =
        -- TODO: Someday check whether functions and externs are used.
        let retInfo = Map.singleton name (mkInfo name ret)
            --argsInfo = Map.fromList $ fmap (\(n, t) -> (n, VariableInfo n False t)) arguments
            argsInfo = Map.fromList $ zipWith (\n t -> (n, mkInfo n t)) arguments argTypes
            externsInfo = Map.mapWithKey mkInfo (L.externs metadata)
        in
        modify \st ->
            st { analyzerSymbols = Map.unions [argsInfo, retInfo, externsInfo, analyzerSymbols st] }
analyze _ (Start _ _ _ _) = lift $ throwE StartIsNotFirstSymbol
analyze _ _ = lift $ throwE StartIsNotFirstSymbol

analyzeImpl :: AST -> LogicAnalyzerT (Maybe L.VariableType)
analyzeImpl = \case
    Assign var expr next -> do
        analyzeAssign var expr
        analyzeImpl next
    End -> pure $ Just L.UnitType
    Expression expr next -> do
        void $ analyzeExpr expr
        analyzeImpl next
    If expr false true next -> do
        analyzeExprTypes "if" L.BoolType expr
        void $ withScope $ analyzeImpl false
        void $ withScope $ analyzeImpl true
        analyzeImpl next
    Return expr -> maybe (pure $ Just L.UnitType) analyzeExpr expr
    Start _ _ _ _ -> lift $ throwE StartIsNotFirstSymbol
    Var var type' expr next -> do
        analyzeVar var type' expr
        analyzeImpl next
    While expr body next -> do
        analyzeExprTypes "while" L.BoolType expr
        void $ withScope $ analyzeImpl body
        analyzeImpl next

analyzeExpr :: Expression -> LogicAnalyzerT (Maybe L.VariableType)
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
        Constant c -> pure $ Just $ vtType c
  where
    markUsed _ info = Just info { unused = False }

    analyzeVariable name = do
        (infoMaybe, symbols) <- gets $
            Map.updateLookupWithKey markUsed name . analyzerSymbols
        modify \st -> st { analyzerSymbols = symbols }
        maybe (addError (UndefinedVariable name) *> pure Nothing)
              (pure . Just . varType)
              infoMaybe
