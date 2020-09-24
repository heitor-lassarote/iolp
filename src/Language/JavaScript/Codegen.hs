{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.JavaScript.Codegen
    ( JavaScriptCodegen
    , Options (..)
    , JSGeneratorState (..)
    , withOptions
    , javaScriptCodegen
    ) where

import Universum

import Control.Monad.Trans.Except (throwE)
import Data.Aeson (FromJSON, ToJSON)
import Data.Default.Class

import Language.Codegen
import Language.Emit
import Language.JavaScript.AST

type JavaScriptCodegen = CodegenT JSGeneratorState

instance Codegen Module where
    type GeneratorState Module = JSGeneratorState

    codegen = javaScriptCodegen

instance Codegen AST where
    type GeneratorState AST = JSGeneratorState

    codegen = genAst

instance Codegen Expression where
    type GeneratorState Expression = JSGeneratorState

    codegen = genExpression

instance Codegen Literal where
    type GeneratorState Literal = JSGeneratorState

    codegen = genLiteral

data Options = Options
    { bracesOnNewLine :: Bool
    , compactCode     :: Bool
    , indentLevel     :: Int
    , strict          :: Bool
    } deriving (Eq, Generic, Show, FromJSON, ToJSON)

instance Default Options where
    def = Options False False 4 True

data JSGeneratorState = JSGeneratorState
    { currentIndentLevel :: Int
    , options            :: Options
    } deriving (Eq, Generic, Show, FromJSON, ToJSON)

instance HasIndentation JSGeneratorState where
    getIndentation = indentLevel . options
    getCurrentIndentation = currentIndentLevel
    setCurrentIndentation l st = st { currentIndentLevel = l }

instance Default JSGeneratorState where
    def = JSGeneratorState 0 def

withOptions :: Options -> JSGeneratorState
withOptions options' = def { options = options' }

indentCompact :: (Emit gen) => JavaScriptCodegen gen
indentCompact = ifM (gets (compactCode . options)) (emitM "") indent

commaSpace :: (Emit gen, Semigroup gen) => JavaScriptCodegen gen
commaSpace = fmap (emit "," <>) space

nl :: (Emit gen) => JavaScriptCodegen gen
nl = ifM (gets (compactCode . options)) (emitM "") (emitM "\n")

space :: (Emit gen) => JavaScriptCodegen gen
space = ifM (gets (compactCode . options)) (emitM "") (emitM " ")

nlIndent :: (Emit gen, Semigroup gen) => JavaScriptCodegen gen
nlIndent = ifM (gets (bracesOnNewLine . options)) (liftA2 (<>) nl indentCompact) space

genLiteral
    :: (Emit gen, Monoid gen)
    => Literal
    -> JavaScriptCodegen gen
genLiteral = \case
    Array xs -> emitBetween' "[" "]" (separatedBy xs =<< commaSpace)
    Boolean x -> emitM if x then "true" else "false"
    Int x -> emitM $ show x
    Number x -> emitM $ show x
    Record fs -> do
        cs <- commaSpace
        emitBetween' "{" "}" $ separatedByF (uncurry codegenField) cs fs
    Text x -> emitBetween' "\"" "\"" $ emitM x
    Void -> lift $ throwE "Cannot print variable of type 'Void'."
  where
    codegenField fieldName expr = mconcat <$> sequence
        [ emitM fieldName
        , emitM ":"
        , space
        , codegen expr
        ]

genBlock
    :: (Emit gen, Monoid gen)
    => [AST]
    -> JavaScriptCodegen gen
genBlock asts = mconcat <$> sequence
    [ nlIndent
    , emitM "{"
    , nl
    , separatedByF (withIndent . genAst) mempty asts
    , indentCompact
    , emitM "}"
    , nl
    ]

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
        , genAst body
        ]
    Nothing -> mconcat <$> sequence
        [ indentCompact
        , emitM "("
        , pure $ mconcat $ map emit args
        , emitM ")"
        , space
        , emitM "=>"
        , space
        , genAst body
        ]

genAssignment
    :: (Emit gen, Monoid gen)
    => Expression
    -> Expression
    -> JavaScriptCodegen gen
genAssignment left right = mconcat <$> sequence
    [ indentCompact
    , genExpression left
    , space
    , emitM "="
    , space
    , genExpression right
    , emitM ";"
    , nl
    ]

genDeclaration
    :: (Emit gen, Monoid gen)
    => Name
    -> Expression
    -> JavaScriptCodegen gen
genDeclaration name expression = mconcat <$> sequence
    [ indentCompact
    , emitM "let "
    , emitIfValid name
    , space
    , emitM "="
    , space
    , genExpression expression
    , emitM ";"
    , nl
    ]

genExpression
    :: (Emit gen, Monoid gen)
    => Expression
    -> JavaScriptCodegen gen
genExpression = \case
    Access expr name -> mconcat <$> sequence
        [ genExpression expr
        , emitM "."
        , emitM name
        ]
    BinaryOp left op right -> mconcat <$> sequence
        [ genExpression left
        , emitM $ binarySymbolToText op
        , genExpression right
        ]
    Call expr args -> mconcat <$> sequence
        [ genExpression expr
        , emitBetween' "(" ")" $ args `separatedBy'` ", "
        ]
    Function nameMaybe args inner -> genFunction nameMaybe args inner
    Index expr inner -> mconcat <$> sequence
        [ genExpression expr
        , emitBetween' "[" "]" $ genExpression inner
        ]
    Literal value -> genLiteral value
    Parenthesis expr -> emitBetween' "(" ")" $ genExpression expr
    UnaryOp op expr -> mconcat <$> sequence
        [ emitM $ unarySymbolToText op
        , genExpression expr
        ]
    Variable name -> emitIfValid name

javaScriptCodegen :: (Emit gen, Monoid gen) => Module -> JavaScriptCodegen gen
javaScriptCodegen m = do
    strict' <- gets (strict . options)
    let useStrict = emit if strict' then "\"use strict\";\n" else ""
    bodies <- traverse genAst (functions m)
    pure $ useStrict <> mconcat bodies

-- TODO: Generate else if instead of ifs inside else blocks.
genAst :: (Emit gen, Monoid gen) => AST -> JavaScriptCodegen gen
genAst = \case
    Assign left right -> genAssignment left right
    Block asts -> genBlock asts
    Expression expression -> mconcat <$> sequence
        [ indentCompact
        , genExpression expression
        , emitM ";"
        , nl
        ]
    If expression t f -> do
        indent' <- indentCompact
        space' <- space
        trueBranch <- genAst t
        falseBranch <- case f of
            Nothing -> emitM mempty
            Just f' -> do
                falseBranch <- genAst f'
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
    Return Nothing -> mconcat <$> sequence
        [ indentCompact
        , emitM "return;"
        , nl
        ]
    Return (Just expression) -> mconcat <$> sequence
        [ indentCompact
        , emitM "return "
        , genExpression expression
        , emitM ";"
        , nl
        ]
    Var name expression -> genDeclaration name expression
    While expression body -> mconcat <$> sequence
        [ indentCompact
        , emitM "while"
        , space
        , emitBetween' "(" ")" $ genExpression expression
        , genAst body
        ]
