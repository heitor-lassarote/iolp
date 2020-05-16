module Language.JavaScript.AST
    ( module Language.Common
    , JSType (..)
    , getName
    , sameType
    , AST (..)
    , Expression (..)
    , symbolToText
    , reservedNames
    , isValidName
    ) where

import Universum

import           Data.Char (isAlphaNum, isLetter)
import qualified Data.Text as T

import Language.Common

data JSType
    = Boolean {-# UNPACK #-} !Bool
    | Number  {-# UNPACK #-} !Double
    | Text                    Text
    deriving (Eq, Show)

getName :: JSType -> Text
getName = \case
    Boolean _ -> "Boolean"
    Number _  -> "Number"
    Text   _  -> "Text"

sameType :: JSType -> JSType -> Bool
sameType left right = case (left, right) of
    (Boolean _, Boolean _) -> True
    (Number  _, Number  _) -> True
    (Text    _, Text    _) -> True
    (        _,         _) -> False

-- Simplified version to be good enough to create the conversion.
data AST
    = Assign Text Expression
    | Block [AST]
    | Expression Expression
    | Function (Maybe Text) [Text] AST
    | If Expression AST (Maybe AST)
    | Var Text Expression
    | While Expression AST

data Expression
    = Call Text [Expression]
    | BinaryOp Expression Symbol Expression
    | Parenthesis Expression
    | UnaryOp Symbol Expression
    | Value (ValueType JSType)

-- Reference:
-- https://www.w3schools.com/js/js_reserved.asp
reservedNames :: [Text]
reservedNames =
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

symbolToText :: Symbol -> Text
symbolToText = \case
    Add          -> "+"
    Divide       -> "/"
    Multiply     -> "*"
    Negate       -> "-"
    Subtract     -> "-"

    Different    -> "!=="
    Equal        -> "==="
    Greater      -> ">"
    GreaterEqual -> ">="
    Less         -> "<"
    LessEqual    -> "<="

    And          -> "&&"
    Not          -> "!"
    Or           -> "||"

isValidName :: Text -> Bool
isValidName name =
    not (T.null name)
    && isLetterUnderscoreOrDollar (T.head name)
    && all (\c -> isAlphaNum c || c == '_' || c == '$') name
    && name `notElem` reservedNames
  where
    isLetterUnderscoreOrDollar c = isLetter c || c == '_' || c == '$'
