module Language.JavaScript.Codegen
    ( JavaScriptCodegen
    , Options (..)
    , defaultOptions
    , JSGeneratorState (..)
    , defaultGeneratorState
    , javaScriptCodegen
    ) where

import Universum

import           Control.Monad.Trans.Except (throwE)
import qualified Data.Set  as S
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
    , strict          :: Bool
    } deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options False False 4 True

data JSGeneratorState = JSGeneratorState
    { currentIndentLevel :: Int
    , options            :: Options
    , symbols            :: Set Text
    } deriving (Eq, Show)

instance HasIndentation JSGeneratorState where
    getIndentation = indentLevel . options
    getCurrentIndentation = currentIndentLevel
    setCurrentIndentation l st = st { currentIndentLevel = l }

defaultGeneratorState :: JSGeneratorState
defaultGeneratorState = JSGeneratorState 0 defaultOptions S.empty

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
    codes <- genLines $ fmap (withIndent . javaScriptCodegenInternal) asts
    space' <- space
    pure $ mconcat
        [ if bracesOnNewLine' then nl' <> indent' else space'
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
    => Maybe Text
    -> [Text]
    -> AST
    -> JavaScriptCodegen gen
genFunction name args body = do
    indent' <- indentCompact
    body' <- javaScriptCodegenInternal body
    space' <- space
    case name of
        Just name' -> pure $ mconcat
            [ indent'
            , emit "function"
            , space'
            , emit name'
            , emit "("
            , mconcat $ map emit args
            , emit ")"
            , body'
            ]
        Nothing -> pure $ mconcat
            [ indent'
            , emit "("
            , mconcat $ map emit args
            , emit ")"
            , space'
            , emit "=>"
            , space'
            , body'
            ]

genAssignmentOrDeclaration
    :: (Emit gen, Monoid gen)
    => Bool
    -> Text
    -> Expression
    -> JavaScriptCodegen gen
genAssignmentOrDeclaration isAssignment name expression = do
    indent' <- indentCompact
    nl' <- nl
    space' <- space
    expression' <- genExpression expression
    pure $ mconcat
        [ indent'
        , emit $ if isAssignment then "" else "let "
        , emit name
        , space'
        , emit "="
        , space'
        , expression'
        , emit ";"
        , nl'
        ]

undefinedVariableError :: Text -> JavaScriptCodegen gen
undefinedVariableError name =
    lift $ throwE $
        "Undefined symbol '" <> name <> "'. Maybe you forgot to declare it?"

checkVariable :: Text -> JavaScriptCodegen ()
checkVariable name = do
    symbols' <- symbols <$> get
    if S.member name symbols' then
        pure ()
    else
        undefinedVariableError name

genVariable
    :: (Emit gen)
    => ValueType JSType
    -> JavaScriptCodegen gen
genVariable (Variable v) = checkVariable v *> (pure $ emit v)
genVariable (Constant c) = pure $ printVariableTy c

genExpression
    :: (Emit gen, Monoid gen)
    => Expression
    -> JavaScriptCodegen gen
genExpression = \case
    Call name args -> do
        indent' <- indentCompact
        nl' <- nl
        args' <- genArgs $ fmap genExpression args
        pure $ mconcat
            [ indent'
            , emit name
            , emit "("
            , args'
            , emit ");"
            , nl'
            ]
    BinaryOp left op right -> do
        left' <- genExpression left
        right' <- genExpression right
        pure $ mconcat
            [ left'
            , emit $ symbolToText op
            , right'
            ]
    UnaryOp op expr -> do
        expr' <- genExpression expr
        pure $ mconcat
            [ emit $ symbolToText op
            , expr'
            ]
    Value value -> genVariable value
  where
    genArgs [] = pure mempty
    genArgs [a] = fmap emit a
    genArgs (a : as) = do
        a' <- a
        as' <- genArgs as
        pure $ emit a' <> emit ", " <> as'

javaScriptCodegen
    :: (Emit gen, Monoid gen)
    => AST
    -> JavaScriptCodegen gen
javaScriptCodegen ast = do
    strict' <- strict . options <$> get
    let useStrict = emit $ if strict' then "\"use strict\";\n" else ""
    body <- javaScriptCodegenInternal ast
    pure $ useStrict <> body

javaScriptCodegenInternal
    :: (Emit gen, Monoid gen)
    => AST
    -> JavaScriptCodegen gen
javaScriptCodegenInternal = \case
    Assign name expression -> genAssignmentOrDeclaration True name expression
    Block asts -> genBlock asts
    Expression expression -> genExpression expression
    Function name args inner -> genFunction name args inner
    If expression t f -> do
        indent' <- indentCompact
        space' <- space
        trueBranch <- go t
        falseBranch <- case f of
            Nothing -> pure $ emit ""
            Just f' -> do
                falseBranch <- go f'
                pure $ mconcat [indent', emit "else", falseBranch]

        expression' <- genExpression expression
        pure $ mconcat
            [ indent'
            , emit "if"
            , space'
            , emit "("
            , expression'
            , emit ")"
            , trueBranch
            , falseBranch
            ]
    Var name expression -> do
        modify $ \st -> st { symbols = S.insert name (symbols st) }
        genAssignmentOrDeclaration False name expression
    While expression body -> do
        indent' <- indentCompact
        space' <- space
        expression' <- genExpression expression
        body' <- go body
        pure $ mconcat
            [ indent'
            , emit "while"
            , space'
            , emit "("
            , expression'
            , emit ")"
            , body'
            ]
  where
    go = javaScriptCodegenInternal
