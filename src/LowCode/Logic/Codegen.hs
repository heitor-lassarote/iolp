module LowCode.Logic.Codegen
    ( VariableType (..)
    , Variable
    , ValueType (..)
    , Comparison (..)
    , LogicAST (..)
    , Options (..)
    , defaultOptions
    , GeneratorState (..)
    , defaultGeneratorState
    ) where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import           Data.Functor.Identity
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as T

import qualified LowCode.Codegen as C
import           LowCode.Emit

type JavaScriptCodegen = StateT GeneratorState (Except Text)

data VariableType
    = FloatTy Float
    | IntegerTy Integer
    | TextTy Text
    deriving (Eq, Show)

type Variable = (Text, VariableType)

data ValueType
    = Variable Text
    | Constant VariableType

data Comparison
    = IsEqual ValueType ValueType
    | IsDifferent ValueType ValueType
    | IsGreaterThan ValueType ValueType
    | IsLessThan ValueType ValueType
    | IsGreaterOrEqualTo ValueType ValueType
    | IsLessOrEqualTo ValueType ValueType

data LogicAST
    = Start LogicAST
    | Var Variable LogicAST
    | If Comparison LogicAST LogicAST LogicAST
    | Print Text LogicAST
    | End

instance C.Codegen LogicAST where
    codegen ast = runIdentity $ runExceptT $ evalStateT (javaScriptCodegen ast) defaultGeneratorState

data Options = Options
    { bracesOnNewLine :: Bool
    , compactCode     :: Bool
    , indentLevel     :: Int
    } deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options False False 4

data GeneratorState = GeneratorState
    { currentIndentLevel :: Int
    , options            :: Options
    , symbols            :: Map.Map Text VariableType
    } deriving (Eq, Show)

defaultGeneratorState :: GeneratorState
defaultGeneratorState = GeneratorState 0 defaultOptions Map.empty

indent :: (Emit gen) => JavaScriptCodegen gen
indent = do
    compactCode <- compactCode . options <$> get
    if compactCode then
        pure $ emit $ ""
    else do
        indent <- currentIndentLevel <$> get
        pure $ emit $ T.replicate indent " "

withIndent :: JavaScriptCodegen gen -> JavaScriptCodegen gen
withIndent action = do
    indent <- indentLevel . options <$> get
    modify $ \st -> st { currentIndentLevel = currentIndentLevel st + indent }
    result <- action
    modify $ \st -> st { currentIndentLevel = currentIndentLevel st - indent }
    pure result

printVariableTy
    :: (Emit gen)
    => VariableType
    -> gen
printVariableTy = emit . \case
    FloatTy x -> T.pack $ show x
    IntegerTy x -> T.pack $ show x
    TextTy x -> x

nl :: (Emit gen) => JavaScriptCodegen gen
nl = do
    compactCode <- compactCode . options <$> get
    pure $ emit $ if compactCode then "" else "\n"

genBlock
    :: (Emit gen, Monoid gen)
    => LogicAST
    -> JavaScriptCodegen gen
genBlock ast = do
    indent' <- indent
    nl' <- nl
    bracesOnNewLine <- bracesOnNewLine . options <$> get
    code <- withIndent $ javaScriptCodegen ast
    pure $ mconcat
        [ if bracesOnNewLine then nl' <> indent' else emit " "
        , emit "{"
        , nl'
        , code
        , indent'
        , emit "}"
        , nl'
        ]

genFunction
    :: (Emit gen, Monoid gen)
    => Text
    -> [Text]
    -> LogicAST
    -> JavaScriptCodegen gen
genFunction name args body = do
    body' <- withIndent $ genBlock body
    pure $ mconcat
        [ emit "function "
        , emit name
        , emit "("
        , mconcat $ map emit args
        , emit ")"
        , body'
        ]

javaScriptCodegen
    :: (Emit gen, Monoid gen)
    => LogicAST
    -> JavaScriptCodegen gen
javaScriptCodegen = \case
    Start l -> genFunction "TODO_addFunctionNames" [] l
    Var (k, v) l -> do
        modify $ \st -> st { symbols = Map.insert k v (symbols st) }
        indent' <- indent
        nl' <- nl
        next <- go l
        pure $ mconcat
            [ indent'
            , emit "var "
            , emit k
            , emit " = "
            , printVariableTy v
            , emit ";"
            , nl'
            , next
            ]
    If p t f l -> do
        let
            get = \case
                Variable v -> emit $ v
                Constant c -> printVariableTy c
            expr = mconcat $ case p of
                IsEqual a b -> [get a, emit " === ", get b]
                IsDifferent a b -> [get a, emit " !== ", get b]
                IsGreaterThan a b -> [get a, emit " > ", get b]
                IsLessThan a b -> [get a, emit " < ", get b]
                IsGreaterOrEqualTo a b -> [get a, emit " >= ", get b]
                IsLessOrEqualTo a b -> [get a, emit " <= ", get b]

        indent' <- indent
        trueBranch <- genBlock t
        falseBranch <- genBlock f
        next <- go l
        pure $ mconcat
            [ indent'
            , emit "if ("
            , expr
            , emit ")"
            , trueBranch
            , indent'
            , emit "else"
            , falseBranch
            , next
            ]
    Print s l -> do
        indent' <- indent
        nl' <- nl
        next <- go l
        pure $ mconcat
            [ indent'
            , emit "alert(\""
            , emit s
            , emit "\");"
            , nl'
            , next
            ]
    End -> pure $ emit ""
  where
    go = javaScriptCodegen
