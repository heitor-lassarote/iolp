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
    , tuple
    , variableName
    ) where

import Universum hiding (Type, bracket, try)

import qualified Data.Char as Char
import qualified Data.Text as Text
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

import Language.Common (Name)

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

tuple :: Parser a -> Parser [a]
tuple p = parenthesis (p `sepBy` lexeme (char ','))

variableName :: Parser Name
variableName = lexeme do
    head' <- letterChar <|> char '_'
    tail' <- takeWhileP Nothing (\c -> Char.isAlphaNum c || c == '_')
    pure $ Text.cons head' tail'
