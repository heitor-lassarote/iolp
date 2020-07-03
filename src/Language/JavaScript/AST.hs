module Language.JavaScript.AST
    ( module Language.Common
    , JSType (..)
    , AST (..)
    , Expression (..)
    , binarySymbolToText
    , unarySymbolToText
    , reservedNames
    , isValidName
    ) where

import Universum

import           Data.Char (isAlphaNum, isLetter)
import           Data.Set  as Set
import qualified Data.Text as T

import Language.Common

data JSType
    = Array    [Expression]
    | Boolean !Bool
    | Number  !Double
    | Text     Text
    | Void
    deriving (Eq, Show)

-- Simplified version good enough to create the conversion.
data AST
    = Assign Text Expression
    | Block [AST]
    | Expression Expression
    | Function (Maybe Text) [Text] AST
    | If Expression AST (Maybe AST)
    | NonScopedBlock [AST]
    | Return (Maybe Expression)
    | Var Text Expression
    | While Expression AST
    deriving (Eq, Show)

data Expression
    = Call Expression [Expression]
    | BinaryOp Expression !BinarySymbol Expression
    | Parenthesis Expression
    | UnaryOp !UnarySymbol Expression
    | Value (ValueType JSType)
    deriving (Eq, Show)

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
