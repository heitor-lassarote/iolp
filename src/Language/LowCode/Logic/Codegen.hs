{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.LowCode.Logic.Codegen
    ( module Language.Codegen
    , Options (..)
    , LogicGeneratorState (..)
    , withOptions
    ) where

import Universum hiding (Type)

import           Data.Default.Class
import qualified Data.Text as Text

import Language.Codegen
import Language.Emit
import Language.LowCode.Logic.AST
import Language.LowCode.Logic.Structure
import Language.LowCode.Logic.Type

type LogicCodegen = CodegenT LogicGeneratorState

data Options = Options
    { bracesOnNewLine :: !Bool
    , compactCode     :: !Bool
    , indentLevel     :: !Int
    } deriving (Eq, Show)

instance Default Options where
    def = Options False False 4

data LogicGeneratorState = LogicGeneratorState
    { currentIndentLevel :: !Int
    , options            :: !Options
    } deriving (Eq, Show)

instance Default LogicGeneratorState where
    def = LogicGeneratorState 0 def

instance HasIndentation LogicGeneratorState where
    getIndentation = indentLevel . options
    getCurrentIndentation = currentIndentLevel
    setCurrentIndentation l st = st { currentIndentLevel = l }

withOptions :: Options -> LogicGeneratorState
withOptions options' = def { options = options' }

indentCompact :: (Emit gen) => LogicCodegen gen
indentCompact = ifM (gets (compactCode . options)) (emitM "") indent

sepSpace :: (Emit gen, Semigroup gen) => Text -> LogicCodegen gen
sepSpace sep = fmap (emit sep <>) space

nl :: (Emit gen) => LogicCodegen gen
nl = ifM (gets (compactCode . options)) (emitM "") (emitM "\n")

space :: (Emit gen) => LogicCodegen gen
space = ifM (gets (compactCode . options)) (emitM "") (emitM " ")

nlIndent :: (Emit gen, Semigroup gen) => LogicCodegen gen
nlIndent = ifM (gets (bracesOnNewLine . options)) (liftA2 (<>) nl indentCompact) space

endl :: (Emit gen, Semigroup gen) => LogicCodegen gen
endl = liftA2 (<>) (emitM ";") nl

instance Codegen MatchPattern where
    type GeneratorState MatchPattern = LogicGeneratorState

    codegen = \case
        DiscardPattern name -> emitM $ Text.cons '?' name
        LiteralPattern lit -> codegen lit
        NamePattern name -> emitM name
        StructurePattern struct -> codegen struct

instance Codegen (Branch e) where
    type GeneratorState (Branch e) = LogicGeneratorState

    codegen = \case
        Branch p [Expression e] -> mconcatA
            [ indentCompact
            , codegen p
            , space
            , emitM "=>"
            , space
            , codegen e
            ]
        Branch p a -> mconcatA
            [ indentCompact
            , codegen p
            , space
            , genBlock a
            ]

genBlock :: (Emit gen, Monoid gen) => [AST exprMetadata] -> LogicCodegen gen
genBlock asts = mconcatA
    [ nlIndent
    , emitM "{"
    , nl
    , separatedByF (withIndent . codegen) mempty asts
    , indentCompact
    , emitM "}"
    , nl
    ]

genIf :: (Emit gen, Monoid gen) => Expression e -> [AST e] -> [AST e] -> LogicCodegen gen
genIf expr'' ifB' elseB' = mconcatA [indent, mkIfElse expr'' ifB' elseB']
  where
    mkIf [If expr ifB elseB] = mkIfElse expr ifB elseB
    mkIf ast                 = genBlock ast

    mkElse []                  = pure mempty
    mkElse [If expr ifB elseB] = mconcatA [indent, emitM "else ", mkIfElse expr ifB elseB]
    mkElse ast                 = mconcatA [indent, emitM "else", genBlock ast]

    mkIfElse expr ifB elseB = mconcatA
        [ emitM "if"
        , space
        , codegen expr
        , mkIf ifB
        , mkElse elseB
        ]

instance Codegen [AST exprMetadata] where
    type GeneratorState [AST exprMetadata] = LogicGeneratorState

    codegen = fmap mconcat . traverse codegen

instance Codegen (AST exprMetadata) where
    type GeneratorState (AST exprMetadata) = LogicGeneratorState

    codegen = \case
        Assign left right -> mconcatA
            [ indentCompact
            , codegen left
            , space
            , emitM "="
            , space
            , codegen right
            , endl
            ]
        Expression expr -> mconcatA
            [ indentCompact
            , codegen expr
            , endl
            ]
        If cond true false -> genIf cond true false
        Match cond branches -> mconcatA
            [ indentCompact
            , emitM "match "
            , codegen cond
            , space
            , mconcatA
                [ nlIndent
                , emitM "{"
                , nl
                , do
                    sep <- sepSpace ","
                    separatedByF (withIndent . codegen) sep branches
                , indentCompact
                , emitM "}"
                ]
            , endl
            ]
        Return Nothing -> mconcatA
            [ indentCompact
            , emitM "return"
            , endl
            ]
        Return (Just expression) -> mconcatA
            [ indentCompact
            , emitM "return "
            , codegen expression
            , endl
            ]
        Var name type' expr -> mconcatA
            [ indentCompact
            , emitM name
            , codegen type'
            , space
            , emitM "="
            , space
            , codegen expr
            , endl
            ]
        While expression body -> mconcatA
            [ indentCompact
            , emitM "while"
            , space
            , codegen expression
            , space
            , genBlock body
            , endl
            ]

instance Codegen (Expression metadata) where
    type GeneratorState (Expression metadata) = LogicGeneratorState

    codegen = \case
        Access _ expr name -> mconcatA
            [ codegen expr
            , emitM "."
            , emitM name
            ]
        BinaryOp _ left symbol' right -> mconcatA
            [ codegen left
            , emitM " "
            , emitM (binaryToText symbol')
            , emitM " "
            , codegen right
            ]
        Call _ expr exprs -> mconcatA
            [ codegen expr
            , emitBetween' "(" ")" $ exprs `separatedBy'` ", "
            ]
        Index _ expr inner -> mconcatA
            [ codegen expr
            , emitBetween' "[" "]" $ codegen inner
            ]
        Literal _ value' -> codegen value'
        Parenthesis _ expr -> emitBetween' "(" ")" $ codegen expr
        Structure _ structure' -> codegen structure'
        UnaryOp _ symbol' expr -> mconcatA
            [ emitM $ unaryToText symbol'
            , codegen expr
            ]
        Variable _ name' -> emitM name'

instance (GeneratorState a ~ LogicGeneratorState, Codegen a) => Codegen (Structure a) where
    type GeneratorState (Structure a) = LogicGeneratorState

    codegen = \case
        Algebraic constructor -> codegen constructor
        Array arr -> do
            sep <- sepSpace ","
            emitBetween' "[" "]" $ arr `separatedBy'` sep
        Record fields -> do
            sep <- sepSpace ","
            emitBetween' "{" "}" $ separatedByF codegen sep fields

instance (GeneratorState a ~ LogicGeneratorState, Codegen a) => Codegen (Field a) where
    type GeneratorState (Field a) = LogicGeneratorState

    codegen (Field fieldName expr) = mconcatA
        [ emitM fieldName
        , space
        , emitM "="
        , space
        , codegen expr
        ]

instance (GeneratorState a ~ LogicGeneratorState, Codegen a) => Codegen (Constructor a) where
    type GeneratorState (Constructor a) = LogicGeneratorState

    codegen (Constructor adtName name value') = mconcatA
        [ emitM adtName
        , emitM "::"
        , emitM name
        , maybe (emitM "") (emitBetween' "(" ")" . codegen) value'
        ]

instance Codegen Literal where
    type GeneratorState Literal = LogicGeneratorState

    codegen = \case
        Char c -> emitBetween' "'" "'" $ emitM (Text.singleton c)
        Double d -> emitM $ show d
        Integer i -> emitM $ show i
        Text t -> emitBetween' "\""  "\"" $ emitM t

instance Codegen Type where
    type GeneratorState Type = LogicGeneratorState

    codegen = \case
        AlgebraicType name -> emitM name
        ArrayType type' -> codegen type'
        CharType -> emitM "Char"
        DoubleType -> emitM "Double"
        FunctionType [arg] ret -> mconcatA
            [ codegen arg
            , space
            , emitM "->"
            , space
            , codegen ret
            ]
        FunctionType args ret -> mconcatA
            [ do
                sep <- sepSpace ","
                emitBetween' "(" ")" $ args `separatedBy` sep
            , space
            , emitM "->"
            , space
            , codegen ret
            ]
        IntegerType -> emitM "Integer"
        RecordType fields -> mconcatA
            [ do
                sep <- sepSpace ","
                emitBetween' "{" "}" $ separatedByF codegen sep fields
            ]
        TextType -> emitM "Text"
