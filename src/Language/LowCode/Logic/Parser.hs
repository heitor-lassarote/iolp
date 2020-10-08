module Language.LowCode.Logic.Parser
    ( Parser
    , spaceConsumer
    , lexeme
    , symbol
    , parse'
    , endl
    , bracket
    , parenthesis
    , braces
    , brackets
    , algebraic
    , array
    , record
    , tuple
    , typeName
    , variableName
    ) where

import Universum hiding (Type, bracket, try)

import qualified Data.Char as Char
import qualified Data.Text as Text
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

import Language.Common (Name)
import Language.LowCode.Logic.Type (Constructor (..), Field (..), Type (..))

type Parser = Parsec Void Text

spaceConsumer :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ Text) => m ()
spaceConsumer = Lexer.space space1 (Lexer.skipLineComment "//") (Lexer.skipBlockCommentNested "/*" "*/")

lexeme :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ Text) => m a -> m a
lexeme = Lexer.lexeme spaceConsumer

symbol :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ Text) => Tokens s -> m (Tokens s)
symbol = Lexer.symbol spaceConsumer

parse' :: Parser a -> Text -> Either Text a
parse' p = first (toText . errorBundlePretty) . parse (space *> p <* eof) ""

endl :: Parser ()
endl = void (lexeme (char ';'))

bracket :: Char -> Char -> Parser a -> Parser a
bracket left right = between (lexeme (char left)) (lexeme (char right))

parenthesis, braces, brackets :: Parser a -> Parser a
parenthesis = bracket '(' ')'
braces      = bracket '[' ']'
brackets    = bracket '{' '}'

algebraic :: Parser a -> Parser (Constructor a)
algebraic p = liftA3 Constructor (try (variableName <* symbol "::")) variableName (optional $ parenthesis p)

array :: Parser a -> Parser [a]
array p = braces (p `sepEndBy` lexeme (char ','))

record :: Parser a -> Parser [Field a]
record p = try $ brackets $ flip sepBy (lexeme (char ',')) do
    fieldName <- variableName
    void (lexeme (char ':'))
    value' <- p
    pure $ Field fieldName value'

tuple :: Parser a -> Parser [a]
tuple p = parenthesis (p `sepBy` lexeme (char ','))

typeName :: Parser Type
typeName = try tupleFunc <|> funcOrType
  where
    types = choice
        [ TextType      <$  symbol "Text"
        , RecordType    <$> record typeName
        , IntegerType   <$  symbol "Integer"
        , DoubleType    <$  symbol "Double"
        , CharType      <$  symbol "Char"
        , ArrayType     <$> braces typeName
        , AlgebraicType <$> variableName
        ]

    right = symbol "->" *> typeName

    funcOrType = do
        left <- parenthesis typeName <|> types
        pure . maybe left (FunctionType [left]) =<< optional right

    tupleFunc = liftA2 FunctionType (tuple typeName) right

variableName :: Parser Name
variableName = lexeme do
    head' <- letterChar <|> char '_'
    tail' <- takeWhileP Nothing (\c -> Char.isAlphaNum c || c == '_')
    pure $ Text.cons head' tail'
