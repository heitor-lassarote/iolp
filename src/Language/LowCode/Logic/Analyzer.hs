module Language.LowCode.Logic.Analyzer
    ( AnalyzerState (..)
    , LogicAnalyzerT
    , emptyState
    , execAnalyzerT
    , analyze
    , analyzeExpr
    ) where

import Universum

import           Control.Monad.Trans.Except (throwE)
import           Data.Data (Constr, toConstr)
import qualified Data.Map.Strict as Map

import Language.LowCode.Logic.AST

-- TODO: Should IncompatibleTypes and TypeMismatch really have these signatures?
-- Or even better: Should TypeMismatch instead have info about the node instead
-- of simply a Text?
data Error
    = IncompatibleTypes Expression Expression
    | ShadowedVariable Text
    | TypeMismatch Text Constr Constr
    | UndefinedVariable Text
    deriving (Eq, Show)

data Warning
    = UnusedVariable Text
    deriving (Eq, Show)

data VariableInfo = VariableInfo
    { unused  :: Bool
    , varType :: Constr
    } deriving (Show)

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
type LogicAnalyzerT = StateT AnalyzerState (ExceptT Error Identity)

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
    modify $ \st -> st
        { analyzerSymbols = symbols
        , warnings = Map.foldrWithKey appendUnused warnings' newSymbols
        }
    pure ret
  where
    appendUnused var info acc =
        if unused info then UnusedVariable var : acc else acc

addError :: Error -> LogicAnalyzerT ()
addError e = modify $ \st -> st { errors = e : errors st }

addWarning :: Warning -> LogicAnalyzerT ()
addWarning w = modify $ \st -> st { warnings = w : warnings st }

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
--    modify $ \st -> st { analyzerSymbols = newSymbols }

--variableCheck :: Text -> LogicAnalyzerT ()
--variableCheck = tryMapVariable id

--markUsed :: Text -> LogicAnalyzerT ()
--markUsed = tryMapVariable (\info -> info { unused = False })

analyzeAssign :: Text -> Expression -> LogicAnalyzerT ()
analyzeAssign var expr = do
    symbols <- gets analyzerSymbols
    constr <- analyzeExpr expr
    case Map.lookup var symbols of
        Nothing -> addError $ UndefinedVariable var
        Just info ->
            if varType info == constr
            then let info' = info { varType = constr }
                  in modify $ \st -> st { analyzerSymbols = Map.insert var info' symbols }
            else addError $ TypeMismatch var (varType info) constr

analyzeVar :: Text -> Expression -> LogicAnalyzerT ()
analyzeVar var expr = do
    symbols <- gets analyzerSymbols
    constr <- analyzeExpr expr
    if (Map.member var symbols)
    then addError $ ShadowedVariable var  -- TODO: Change type?
    else
        let info = VariableInfo { unused = True, varType = constr }
         in modify $ \st -> st { analyzerSymbols = Map.insert var info symbols }

analyzeExprTypes :: Constr -> Expression -> LogicAnalyzerT ()
analyzeExprTypes expectedType expr = do
    constr <- analyzeExpr expr
    if expectedType == constr
    then pure ()
    else addError $ TypeMismatch "while" expectedType constr

analyze :: AST -> LogicAnalyzerT ()
analyze ast = withScope $ analyzeImpl ast

analyzeImpl :: AST -> LogicAnalyzerT ()
analyzeImpl = \case
    Assign var expr next -> do
        analyzeAssign var expr
        analyzeImpl next
    End _ -> pure ()  -- TODO: Implement this!
    If expr false true next -> do
        analyzeExprTypes expectedBool expr
        void $ withScope $ analyzeImpl false
        void $ withScope $ analyzeImpl true
        analyzeImpl next
    Print _ next -> do
        analyzeImpl next
    Start _ next -> analyzeImpl next
    Var var expr next -> do
        analyzeVar var expr
        analyzeImpl next
    While expr body next -> do
        analyzeExprTypes expectedBool expr
        void $ withScope $ analyzeImpl body
        analyzeImpl next
  where
    expectedBool = toConstr $ BoolTy False

analyzeExpr :: Expression -> LogicAnalyzerT Constr
analyzeExpr = \case
    BinaryOp left _ right -> do
        typeL <- analyzeExpr left
        typeR <- analyzeExpr right
        if typeL == typeR
        then pure typeL
        else lift $ throwE $ IncompatibleTypes left right
    Parenthesis expr -> analyzeExpr expr
    UnaryOp _ expr -> analyzeExpr expr
    Value value -> case value of
        Variable v -> do
            (infoMaybe, symbols) <- gets $
                Map.updateLookupWithKey markUsed v . analyzerSymbols
            modify $ \st -> st { analyzerSymbols = symbols }
            maybe (lift $ throwE $ UndefinedVariable v) (pure . varType) infoMaybe
        Constant c -> pure $ toConstr c
  where
    markUsed _ info = Just info { unused = False }
