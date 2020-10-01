module Language.JavaScript.AST
    ( module Language.Common
    , Module (..)
    , AST (..)
    , Expression (..)
    , Literal (..)
    , binarySymbolToText
    , unarySymbolToText
    , reservedNames
    , isValidName
    ) where

import Universum hiding (Const)

import           Data.Char (isAlphaNum, isLetter)
import           Data.Set  as Set
import qualified Data.Text as T

import Language.Common

data Module = Module
    { functions :: ![AST]
    } deriving (Eq, Ord, Show)

-- Simplified version good enough to create the conversion.
data AST
    = Assign !Expression !Expression
    | Block ![AST]
    | Const !Text !Expression
    | Expression !Expression
    | If !Expression !AST !(Maybe AST)
    | Return !(Maybe Expression)
    | Throw !Expression
    | Var !Text !Expression
    | While !Expression !AST
    deriving (Eq, Ord, Show)

data Expression
    = Access !Expression !Name
    | BinaryOp !Expression !BinarySymbol !Expression
    | Call !Expression ![Expression]
    | Function !(Maybe Text) ![Text] !AST
    | Index !Expression !Expression
    | Literal !Literal
    | Parenthesis !Expression
    | UnaryOp !UnarySymbol !Expression
    | Variable !Name
    deriving (Eq, Ord, Show)

data Literal
    = Array   ![Expression]
    | Boolean !Bool
    | Int     !Int
    | Number  !Double
    | Record  ![(Name, Expression)]
    | Text    !Text
    deriving (Eq, Ord, Show)

-- Reference:
-- https://www.w3schools.com/js/js_reserved.asp
reservedNames :: Set Text
reservedNames = Set.fromDistinctAscList
    [ "abstract", "arguments", "await", "boolean", "break", "byte", "case"
    , "catch", "char", "class", "const", "continue", "debugger", "default"
    , "delete", "do", "double", "else", "enum", "eval", "export", "extends"
    , "false", "final", "finally", "float", "for", "function", "goto", "if"
    , "implements", "import", "in", "instanceof", "int", "interface", "let"
    , "long", "native", "new", "null", "package", "private", "protected"
    , "public", "return", "short", "static", "super", "switch", "synchronized"
    , "this", "throw", "throws", "transient", "true", "try", "typeof", "var"
    , "void", "volatile", "while", "with", "yield"
    ]

binarySymbolToText :: BinarySymbol -> Text
binarySymbolToText = \case
    Add          -> "+"
    Divide       -> "/"
    Multiply     -> "*"
    Subtract     -> "-"
    Different    -> "!=="
    Equal        -> "==="
    Greater      -> ">"
    GreaterEqual -> ">="
    Less         -> "<"
    LessEqual    -> "<="
    And          -> "&&"
    Or           -> "||"

unarySymbolToText  :: UnarySymbol -> Text
unarySymbolToText = \case
    Negate       -> "-"
    Not          -> "!"

isValidName :: Text -> Bool
isValidName name =
    not (T.null name)
    && isLetterUnderscoreOrDollar (T.head name)
    && all (\c -> isAlphaNum c || c == '_' || c == '$') name
    && name `Set.notMember` reservedNames
  where
    isLetterUnderscoreOrDollar c = isLetter c || c == '_' || c == '$'
