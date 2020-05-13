module Language.JavaScript.Codegen where

import Universum

import qualified Data.Map  as M
import qualified Data.Text as T

import Language.Codegen
import Language.Emit
import Language.JavaScript.AST

type JavaScriptCodegen = CodegenT JSGeneratorState

instance Codegen AST where
    type GeneratorState AST = JSGeneratorState
    codegen = javaScriptCodegen

data Options = Options
    { bracesOnNewLine :: Bool
    , compactCode     :: Bool
    , indentLevel     :: Int
    } deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options False False 4

data JSGeneratorState = JSGeneratorState
    { currentIndentLevel :: Int
    , options            :: Options
    , symbols            :: Map Text JSType
    } deriving (Eq, Show)

instance HasIndentation JSGeneratorState where
    getIndentation = indentLevel . options
    getCurrentIndentation = currentIndentLevel
    setCurrentIndentation l st = st { currentIndentLevel = l }

defaultGeneratorState :: JSGeneratorState
defaultGeneratorState = JSGeneratorState 0 defaultOptions M.empty

indentCompact :: (Emit gen) => JavaScriptCodegen gen
indentCompact = do
    compactCode' <- compactCode . options <$> get
    if compactCode' then
        pure $ emit ""
    else
        indent

printVariableTy
    :: (Emit gen)
    => JSType
    -> gen
printVariableTy = emit . \case
    Number x -> show x
    Text x -> x

nl :: (Emit gen) => JavaScriptCodegen gen
nl = do
    compactCode' <- compactCode . options <$> get
    pure $ emit $ if compactCode' then "" else "\n"

space :: (Emit gen) => JavaScriptCodegen gen
space = do
    compactCode' <- compactCode . options <$> get
    pure $ emit $ if compactCode' then "" else " "

genBlock
    :: (Emit gen, Monoid gen)
    => [AST]
    -> JavaScriptCodegen gen
genBlock asts = do
    bracesOnNewLine' <- bracesOnNewLine . options <$> get
    indent' <- indentCompact
    nl' <- nl
    codes <- genLines $ fmap (withIndent . javaScriptCodegen) asts
    space' <- space
    pure $ mconcat
        [ if bracesOnNewLine' then nl' <> indent' else space'
        , emit "{"
        , nl'
        , codes
        , indent'
        , emit "}"
        , if null asts then emit "" else nl'
        ]
  where
    genLines [] = pure $ emit mempty
    genLines (code : codes) = do
        code' <- code
        codes' <- genLines codes
        pure $ code' <> codes'

genFunction
    :: (Emit gen, Monoid gen)
    => Maybe Text
    -> [Text]
    -> AST
    -> JavaScriptCodegen gen
genFunction name args body = do
    body' <- withIndent $ javaScriptCodegen body
    space' <- space
    case name of
        Just name' -> pure $ mconcat
            [ emit "function"
            , space'
            , emit name'
            , emit "("
            , mconcat $ map emit args
            , emit ")"
            , body'
            ]
        Nothing -> pure $ mconcat
            [ emit "("
            , mconcat $ map emit args
            , emit ")"
            , space'
            , emit "=>"
            , space'
            , body'
            ]

genEqualityOp
    :: (Emit gen)
    => EqualityOp
    -> gen
genEqualityOp = emit . \case
    IsEqual -> "==="
    IsDifferent -> "!=="
    IsGreaterThan -> ">"
    IsLessThan -> "<"
    IsGreaterOrEqualTo -> ">="
    IsLessOrEqualTo -> "<="

javaScriptCodegen
    :: (Emit gen, Monoid gen)
    => AST
    -> JavaScriptCodegen gen
javaScriptCodegen = \case
    Block asts -> genBlock asts
    Call name args -> do
        indent' <- indentCompact
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
    If (left, op, right) t f -> do
        let
            getVar = \case
                Variable v -> emit $ v
                Constant c -> printVariableTy c

        indent' <- indentCompact
        space' <- space
        trueBranch <- go t
        falseBranch <- case f of
            Nothing -> pure $ emit ""
            Just f' -> do
                falseBranch <- go f'
                pure $ mconcat [indent', emit "else", falseBranch]
        pure $ mconcat
            [ indent'
            , emit "if"
            , space'
            , emit "("
            , getVar left
            , space'
            , genEqualityOp op
            , space'
            , getVar right
            , emit ")"
            , trueBranch
            , falseBranch
            ]
    Var k v -> do
        modify $ \st -> st { symbols = M.insert k v (symbols st) }
        indent' <- indentCompact
        nl' <- nl
        space' <- space
        pure $ mconcat
            [ indent'
            , emit "var "
            , emit k
            , space'
            , emit "="
            , space'
            , printVariableTy v
            , emit ";"
            , nl'
            ]
  where
    go = javaScriptCodegen
