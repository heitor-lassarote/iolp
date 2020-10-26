{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.JavaScript.Codegen
    ( JavaScriptCodegen
    , Options (..)
    , JSGeneratorState (..)
    , withOptions
    , javaScriptCodegen
    ) where

import Universum hiding (Const)

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
    { bracesOnNewLine :: !Bool
    , compactCode     :: !Bool
    , indentLevel     :: !Int
    , strict          :: !Bool
    } deriving (Eq, Generic, Show, FromJSON, ToJSON)

instance Default Options where
    def = Options False False 4 True

data JSGeneratorState = JSGeneratorState
    { currentIndentLevel :: !Int
    , options            :: !Options
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

sepSpace :: (Emit gen, Semigroup gen) => Text -> JavaScriptCodegen gen
sepSpace sep = fmap (emit sep <>) space

nl :: (Emit gen) => JavaScriptCodegen gen
nl = ifM (gets (compactCode . options)) (emitM "") (emitM "\n")

space :: (Emit gen) => JavaScriptCodegen gen
space = ifM (gets (compactCode . options)) (emitM "") (emitM " ")

nlIndent :: (Emit gen, Semigroup gen) => JavaScriptCodegen gen
nlIndent = ifM (gets (bracesOnNewLine . options)) (liftA2 (<>) nl indentCompact) space

endl :: (Emit gen, Semigroup gen) => JavaScriptCodegen gen
endl = liftA2 (<>) (emitM ";") nl

genLiteral
    :: (Emit gen, Monoid gen)
    => Literal
    -> JavaScriptCodegen gen
genLiteral = \case
    Array xs -> emitBetween' "[" "]" (separatedBy xs =<< sepSpace ",")
    Boolean x -> emitM if x then "true" else "false"
    Int x -> emitM $ show x
    Number x -> emitM $ show x
    Record fs -> do
        cs <- sepSpace ","
        emitBetween' "{" "}" $ separatedByF (uncurry codegenField) cs fs
    Text x -> emitBetween' "\"" "\"" $ emitM x
  where
    codegenField fieldName expr = mconcatA
        [ emitM fieldName
        , emitM ":"
        , space
        , codegen expr
        ]

genBlock
    :: (Emit gen, Monoid gen)
    => [AST]
    -> JavaScriptCodegen gen
genBlock asts = mconcatA
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
    Just name' -> mconcatA
        [ indentCompact
        , emitM "function"
        , space
        , emitIfValid name'
        , args'
        , body'
        , nl
        ]
    Nothing -> mconcatA
        [ indentCompact
        , args'
        , space
        , emitM "=>"
        , space
        , body'
        , nl
        ]
  where
    args' = emitBetween' "(" ")" $ mconcatA $ intersperse (sepSpace ",") $ map emitM args
    body' = case body of
        -- This case is handled separately, because an arrow function to a record
        -- is mistaken by JS as an arrow function to a block with label syntax.
        Expression (Literal (Record fields)) -> emitBetween' "(" ");" $ genLiteral $ Record fields
        _                                    -> genAst body

genAssignment
    :: (Emit gen, Monoid gen)
    => Expression
    -> Expression
    -> JavaScriptCodegen gen
genAssignment left right = mconcatA
    [ indentCompact
    , genExpression left
    , space
    , emitM "="
    , space
    , genExpression right
    ]

genDeclaration
    :: (Emit gen, Monoid gen)
    => Bool
    -> Name
    -> Expression
    -> JavaScriptCodegen gen
genDeclaration isConst name expression = mconcatA
    [ indentCompact
    , emitM if isConst then "const " else "let "
    , emitIfValid name
    , space
    , emitM "="
    , space
    , genExpression expression
    ]

genExpression
    :: (Emit gen, Monoid gen)
    => Expression
    -> JavaScriptCodegen gen
genExpression = \case
    Access expr name -> mconcatA
        [ genExpression expr
        , emitM "."
        , emitM name
        ]
    BinaryOp left op right -> mconcatA
        [ genExpression left
        , space
        , emitM $ binarySymbolToText op
        , space
        , genExpression right
        ]
    Call expr args -> mconcatA
        [ genExpression expr
        , do
            sep <- sepSpace ","
            emitBetween' "(" ")" $ args `separatedBy` sep
        ]
    Function nameMaybe args inner -> genFunction nameMaybe args inner
    Index expr inner -> mconcatA
        [ genExpression expr
        , emitBetween' "[" "]" $ genExpression inner
        ]
    Literal value -> genLiteral value
    Parenthesis expr -> emitBetween' "(" ")" $ genExpression expr
    UnaryOp op expr -> mconcatA
        [ emitM $ unarySymbolToText op
        , genExpression expr
        ]
    Variable name -> emitIfValid name

javaScriptCodegen :: (Emit gen, Monoid gen) => Module -> JavaScriptCodegen gen
javaScriptCodegen m = do
    strict' <- gets (strict . options)
    let useStrict = emitM if strict' then "\"use strict\";\n" else ""
    liftA2 (<>) useStrict (genModule m)

genModule :: (Emit gen, Monoid gen) => Module -> JavaScriptCodegen gen
genModule (Module functions) = mconcat <$> traverse genAst functions

genIf :: (Emit gen, Monoid gen) => Expression -> AST -> Maybe AST -> JavaScriptCodegen gen
genIf expr'' ifB' elseB' = mconcatA [indent, mkIfElse expr'' ifB' elseB']
  where
    mkIf (Block [If expr ifB elseB]) = mkIfElse expr ifB elseB
    mkIf b                           = genAst b

    mkElse Nothing  = pure mempty
    mkElse (Just f) = case f of
        Block [If expr ifB elseB] -> mconcatA [indent, emitM "else ", mkIfElse expr ifB elseB]
        If expr ifB elseB         -> mconcatA [indent, emitM "else ", mkIfElse expr ifB elseB]
        _                         -> mconcatA [indent, emitM "else", genAst f]

    mkIfElse expr ifB elseB = mconcatA
        [ emitM "if"
        , space
        , emitBetween' "(" ")" $ genExpression expr
        , mkIf ifB
        , mkElse elseB
        ]

genAst :: (Emit gen, Monoid gen) => AST -> JavaScriptCodegen gen
genAst = \case
    Assign left right@(Function _ _ _) -> genAssignment left right
    Assign left right -> liftA2 (<>) (genAssignment left right) endl
    Block asts -> genBlock asts
    Const left right@(Function _ _ _) -> genDeclaration True left right
    Const left right -> liftA2 (<>) (genDeclaration True left right) endl
    Expression expression@(Function _ _ _) -> liftA2 (<>) indentCompact (genExpression expression)
    Expression expression -> mconcatA
        [ indentCompact
        , genExpression expression
        , endl
        ]
    If expression t f -> genIf expression t f
    Return Nothing -> mconcatA
        [ indentCompact
        , emitM "return"
        , endl
        ]
    Return (Just expression) -> mconcatA
        [ indentCompact
        , emitM "return "
        , genExpression expression
        , endl
        ]
    Throw expression -> mconcatA
        [ indentCompact
        , emitM "throw new "
        , genExpression expression
        , endl
        ]
    Var left right@(Function _ _ _) -> genDeclaration False left right
    Var left right -> liftA2 (<>) (genDeclaration False left right) endl
    While expression body -> mconcatA
        [ indentCompact
        , emitM "while"
        , space
        , emitBetween' "(" ")" $ genExpression expression
        , genAst body
        ]
