{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.JavaScript.Codegen
    ( JavaScriptCodegen
    , Options (..)
    , defaultOptions
    , JSGeneratorState (..)
    , defaultGeneratorState
    , withOptions
    , javaScriptCodegen
    ) where

import Universum

import           Control.Monad.Trans.Except (throwE)
import           Data.Aeson (FromJSON, ToJSON)
import qualified Data.Set  as S

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
    } deriving (Eq, Generic, Show)

instance FromJSON Options
instance ToJSON   Options

defaultOptions :: Options
defaultOptions = Options False False 4 True

data JSGeneratorState = JSGeneratorState
    { currentIndentLevel :: Int
    , options            :: Options
    , symbols            :: Set Text
    } deriving (Eq, Generic, Show)

instance FromJSON JSGeneratorState
instance ToJSON   JSGeneratorState

instance HasIndentation JSGeneratorState where
    getIndentation = indentLevel . options
    getCurrentIndentation = currentIndentLevel
    setCurrentIndentation l st = st { currentIndentLevel = l }

defaultGeneratorState :: JSGeneratorState
defaultGeneratorState = JSGeneratorState 0 defaultOptions S.empty

withOptions :: Options -> JSGeneratorState
withOptions options' = defaultGeneratorState { options = options' }

indentCompact :: (Emit gen) => JavaScriptCodegen gen
indentCompact = do
    compactCode' <- gets (compactCode . options)
    if compactCode' then emitM "" else indent

genConstant
    :: (Emit gen)
    => JSType
    -> JavaScriptCodegen gen
genConstant = \case
    Boolean x -> emitM $ show x
    Number x -> emitM $ show x
    Text x -> emitM x
    Void -> lift $ throwE "Cannot print variable of type 'Void'."

nl :: (Emit gen) => JavaScriptCodegen gen
nl = do
    compactCode' <- gets (compactCode . options)
    emitM if compactCode' then "" else "\n"

space :: (Emit gen) => JavaScriptCodegen gen
space = do
    compactCode' <- gets (compactCode . options)
    emitM if compactCode' then "" else " "

genBlock
    :: (Emit gen, Monoid gen)
    => [AST]
    -> JavaScriptCodegen gen
genBlock asts = do
    bracesOnNewLine' <- gets (bracesOnNewLine . options)
    indent' <- indentCompact
    nl' <- nl
    codes <- genLines asts
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
    genLines :: (Emit gen, Monoid gen) => [AST] -> JavaScriptCodegen gen
    genLines = fmap mconcat . traverse (withIndent . javaScriptCodegenInternal)

emitIfValid :: (Emit gen) => Name -> JavaScriptCodegen gen
emitIfValid name
    | isValidName name = emitM name
    | otherwise        = lift $ throwE $ "Invalid identifier name '" <> name <> "'."

genFunction
    :: (Emit gen, Monoid gen)
    => Maybe Name
    -> [Name]
    -> AST
    -> JavaScriptCodegen gen
genFunction name args body = case name of
    Just name' -> mconcat <$> sequence
        [ indentCompact
        , emitM "function"
        , space
        , emitIfValid name'
        , emitM "("
        , pure $ mconcat $ map emit args
        , emitM ")"
        , javaScriptCodegenInternal body
        ]
    Nothing -> mconcat <$> sequence
        [ indentCompact
        , emitM "("
        , pure $ mconcat $ map emit args
        , emitM ")"
        , space
        , emitM "=>"
        , space
        , javaScriptCodegenInternal body
        ]

genAssignmentOrDeclaration
    :: (Emit gen, Monoid gen)
    => Bool
    -> Name
    -> Expression
    -> JavaScriptCodegen gen
genAssignmentOrDeclaration isAssignment name expression = mconcat <$> sequence
    [ indentCompact
    , emitM if isAssignment then "" else "let "
    , emitIfValid name
    , space
    , emitM "="
    , space
    , genExpression expression
    , emitM ";"
    , nl
    ]

undefinedVariableError :: Name -> JavaScriptCodegen gen
undefinedVariableError name =
    lift $ throwE $
        "Undefined symbol '" <> name <> "'. Maybe you forgot to declare it?"

checkVariable :: Name -> JavaScriptCodegen ()
checkVariable name = do
    symbols' <- gets symbols
    if S.member name symbols' then
        pure ()
    else
        undefinedVariableError name

genVariable
    :: (Emit gen)
    => ValueType JSType
    -> JavaScriptCodegen gen
genVariable (Variable v) = checkVariable v *> emitIfValid v
genVariable (Constant c) = genConstant c

genExpression
    :: (Emit gen, Monoid gen)
    => Expression
    -> JavaScriptCodegen gen
genExpression = \case
    Call expr args -> mconcat <$> sequence
        [ indentCompact
        , genExpression expr
        , emitM "("
        , fmap (mconcat . fmap emit . intersperse ", ") $ traverse genExpression args
        , emitM ");"
        , nl
        ]
    BinaryOp left op right -> mconcat <$> sequence
        [ genExpression left
        , emitM $ binarySymbolToText op
        , genExpression right
        ]
    Parenthesis expr -> mconcat <$> sequence
        [ emitM "("
        , genExpression expr
        , emitM ")"
        ]
    UnaryOp op expr -> mconcat <$> sequence
        [ emitM $ unarySymbolToText op
        , genExpression expr
        ]
    Value value -> genVariable value

javaScriptCodegen
    :: (Emit gen, Monoid gen)
    => AST
    -> JavaScriptCodegen gen
javaScriptCodegen ast = do
    strict' <- gets $ strict . options
    let useStrict = emit if strict' then "\"use strict\";\n" else ""
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
            Nothing -> emitM mempty
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
    NonScopedBlock asts -> mconcat <$> traverse javaScriptCodegenInternal asts
    Return Nothing -> mconcat <$> sequence
        [ indentCompact
        , emitM "return;"
        ]
    Return (Just expression) -> mconcat <$> sequence
        [ indentCompact
        , emitM "return "
        , genExpression expression
        , emitM ";"
        ]
    Var name expression -> do
        modify \st -> st { symbols = S.insert name (symbols st) }
        genAssignmentOrDeclaration False name expression
    While expression body -> mconcat <$> sequence
        [ indentCompact
        , emitM "while"
        , space
        , emitM "("
        , genExpression expression
        , emitM ")"
        , go body
        ]
  where
    go = javaScriptCodegenInternal
