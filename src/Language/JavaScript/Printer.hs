module Language.JavaScript.Printer where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import           Data.Functor.Identity
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as T

import qualified Language.Codegen as C
import           Language.Emit
import           Language.JavaScript.AST

type JavaScriptCodegen = StateT GeneratorState (Except Text)

instance C.Codegen AST where
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
    , symbols            :: Map.Map Text JSType
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
    => JSType
    -> gen
printVariableTy = emit . \case
    Number x -> T.pack $ show x
    Text x -> x

nl :: (Emit gen) => JavaScriptCodegen gen
nl = do
    compactCode <- compactCode . options <$> get
    pure $ emit $ if compactCode then "" else "\n"

genBlock
    :: (Emit gen, Monoid gen)
    => [AST]
    -> JavaScriptCodegen gen
genBlock asts = do
    bracesOnNewLine <- bracesOnNewLine . options <$> get
    indent' <- indent
    nl' <- nl
    codes <- genLines $ fmap (withIndent . javaScriptCodegen) asts
    pure $ mconcat
        [ if bracesOnNewLine then nl' <> indent' else emit " "
        , emit "{"
        , nl'
        , codes
        , indent'
        , emit "}"
        , nl'
        ]
  where
    genLines [] = pure $ emit mempty
    genLines (code : codes) = do
        code' <- code
        codes' <- genLines codes
        pure $ code' <> codes'

genFunction
    :: (Emit gen, Monoid gen)
    => Text
    -> [Text]
    -> AST
    -> JavaScriptCodegen gen
genFunction name args body = do
    body' <- withIndent $ javaScriptCodegen body
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
    => AST
    -> JavaScriptCodegen gen
javaScriptCodegen = \case
    Block asts -> genBlock asts
    Call name args -> do
        indent' <- indent
        nl' <- nl
        pure $ mconcat
            [ indent'
            , emit name
            , emit "("
            , emit $ T.intercalate "," args
            , emit ");"
            , nl'
            ]
    Function name args inner -> genFunction name args inner
    If p t f -> do
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
        trueBranch <- go t
        falseBranch <- case f of
            Nothing -> pure $ emit ""
            Just f' -> do
                falseBranch <- go f'
                pure $ mconcat [indent', emit "else", falseBranch]
        pure $ mconcat
            [ indent'
            , emit "if ("
            , expr
            , emit ")"
            , trueBranch
            , falseBranch
            ]
    Var k v -> do
        modify $ \st -> st { symbols = Map.insert k v (symbols st) }
        indent' <- indent
        nl' <- nl
        pure $ mconcat
            [ indent'
            , emit "var "
            , emit k
            , emit " = "
            , printVariableTy v
            , emit ";"
            , nl'
            ]
  where
    go = javaScriptCodegen
