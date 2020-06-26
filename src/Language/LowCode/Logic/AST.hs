module Language.LowCode.Logic.AST
    ( Metadata (..)
    , AST (..)
    , Expression (..)
    , parseExpression
    ) where

import           Universum hiding (bool, many, take, takeWhile, try)
import qualified Universum.Unsafe as Unsafe

import           Control.Monad.Trans.Except (throwE)
import           Data.Aeson
import           Data.Char (isAlphaNum)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

import           Language.Codegen
import           Language.Common
import           Language.Emit
import qualified Language.LowCode.Logic.Types as L

newtype Metadata = Metadata
    { externs :: Map Name L.VariableType
    } deriving (Eq, Generic, Show)

instance FromJSON Metadata
instance ToJSON   Metadata


data AST
    -- | Assigns a new value to the variable with the specified name and type
    -- and what follows after.
    = Assign Name Expression AST
    -- | Represents the end of a cycle.
    | End
    -- | Executes a raw expression. Useful for Call.
    | Expression Expression AST
    -- | Allows a condition to be tested, and contains the true branch, false
    -- branch and what follows after the branches.
    | If Expression AST AST AST
    -- | Exits the function, optionally returning a value.
    | Return (Maybe Expression)
    -- | Represents the start of a cycle with a given name and a list of
    -- parameters.
    | Start Name !L.VariableType [Name] AST
    -- | Declares a variable with the specified name and type, followed by the
    -- remainder of the cycle.
    | Var Name !L.VariableType Expression AST
    -- | Represents a condition which should be run while a predicate is not
    -- met, followed by the remainder of the cycle.
    | While Expression AST AST
    deriving (Eq, Show)

instance FromJSON AST where
    parseJSON = withObject "logic ast" \o -> o .: "tag" >>= \case
        "assign"      -> Assign     <$> o .:  "name"
                                    <*> o .:  "expression"
                                    <*> o .:? "next-ast"         .!= End
        "expression"  -> Expression <$> o .:  "expression"
                                    <*> o .:? "next-ast"         .!= End
        "if"          -> If         <$> o .:  "expression"
                                    <*> o .:  "true-branch-ast"
                                    <*> o .:? "false-branch-ast" .!= End
                                    <*> o .:? "next-ast"         .!= End
        "return"      -> Return     <$> o .:? "expression"
        "start"       -> Start      <$> o .:  "name"
                                    <*> o .:  "return-type"
                                    <*> o .:  "arguments"
                                    <*> o .:? "next-ast"         .!= End
        "var"         -> Var        <$> o .:  "name"
                                    <*> o .:  "type"
                                    <*> o .:  "expression"
                                    <*> o .:? "next-ast"         .!= End
        "while"       -> While      <$> o .:  "expression"
                                    <*> o .:  "while-ast"
                                    <*> o .:? "next-ast"         .!= End
        other         -> fail $ "Unknown tag '"
                            <> other
                            <> "'. Available tags are: 'assign', 'expression'"
                            <> ", 'if', 'return', 'start', 'var' and 'while'."

instance ToJSON AST where
    toJSON = \case
        Assign name expression ast -> object
            [ "tag"              .= String "assign"
            , "name"             .= String name
            , "expression"       .= expression
            , "next-ast"         .= ast
            ]
        End -> Null
        Expression expression ast -> object
            [ "tag"              .= String "expression"
            , "expression"       .= expression
            , "next-ast"         .= ast
            ]
        If expression true false ast -> object
            [ "tag"              .= String "if"
            , "expression"       .= expression
            , "true-branch-ast"  .= true
            , "false-branch-ast" .= false
            , "next-ast"         .= ast
            ]
        Return expression -> object
            [ "tag"              .= String "return"
            , "expression"       .= expression
            ]
        Start name returnType arguments ast -> object
            [ "tag"              .= String "start"
            , "name"             .= String name
            , "return-type"      .= returnType
            , "arguments"        .= arguments
            , "next-ast"         .= ast
            ]
        Var name type' expression ast -> object
            [ "tag"              .= String "var"
            , "name"             .= String name
            , "type"             .= type'
            , "expression"       .= expression
            , "next-ast"         .= ast
            ]
        While expression body ast -> object
            [ "tag"              .= String "while"
            , "expression"       .= expression
            , "while-ast"        .= body
            , "next-ast"         .= ast
            ]

data Expression
    = BinaryOp Expression !BinarySymbol Expression
    | Call Expression [Expression]
    | Parenthesis Expression
    | UnaryOp !UnarySymbol Expression
    | Value (ValueType L.Variable)
    deriving (Eq, Show)

instance FromJSON Expression where
    parseJSON = withText "expression ast" (either (fail . toString) pure . parseExpression)

instance ToJSON Expression where
    toJSON = String . Unsafe.fromJust . rightToMaybe . evalCodegenT () . codegen

instance Codegen Expression where
    type GeneratorState Expression = ()

    codegen = \case
        BinaryOp left symbol' right -> mconcat <$> sequence
            [ codegen left
            , tryBinaryText symbol'
            , codegen right
            ]
        Call expr exprs -> mconcat <$> sequence
            [ codegen expr
            , emitM "("
            , mconcat . fmap emit . intersperse "," <$> traverse codegen exprs
            , emitM ")"
            ]
        Parenthesis expression -> mconcat <$> sequence
            [ emitM "("
            , codegen expression
            , emitM ")"
            ]
        UnaryOp symbol' expression -> mconcat <$> sequence
            [ tryUnaryText symbol'
            , codegen expression
            ]
        Value value' -> pure case value' of
            Variable variable' -> emit variable'
            Constant constant' -> case constant' of
                L.Bool b -> emit $ show b
                L.Char c -> emit "'" <> emit (Text.singleton c) <> emit "'"
                L.Double d -> emit $ show d
                L.Integer i -> emit $ show i
                L.Text t -> emit "\"" <> emit t <> emit "\""
                L.Unit -> emit "()"
      where
        tryText map' sym =
            maybe (lift $ throwE $ errorUnaryNotFound $ show sym) emitM $
                Map.lookup sym map'
        tryUnaryText = tryText unarySymbolToText
        tryBinaryText = tryText binarySymbolToText

data Associativity = LeftAssoc | RightAssoc deriving (Eq, Show)
type Precedence = Int

data Operator = Operator
    { associativity :: !Associativity
    , precedence    :: !Precedence
    } deriving (Eq, Show)

-- Reference:
-- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence
binarySymbolToOperator :: BinarySymbol -> Operator
binarySymbolToOperator = \case
    Add          -> Operator LeftAssoc 14
    Divide       -> Operator LeftAssoc 15
    Multiply     -> Operator LeftAssoc 15
    Subtract     -> Operator LeftAssoc 14
    Different    -> Operator LeftAssoc 11
    Equal        -> Operator LeftAssoc 11
    Greater      -> Operator LeftAssoc 12
    GreaterEqual -> Operator LeftAssoc 12
    Less         -> Operator LeftAssoc 12
    LessEqual    -> Operator LeftAssoc 12
    And          -> Operator LeftAssoc  6
    Or           -> Operator LeftAssoc  5

type Parser = Parsec Void Text

parseExpression :: Text -> Either Text Expression
parseExpression = first (toText . errorBundlePretty) . parse (expression0 <* eof) ""

-- Reference:
-- https://en.wikipedia.org/wiki/Operator-precedence_parser#Precedence_climbing_method
expression0 :: Parser Expression
expression0 = expression1 0 =<< nonBinary

expression1 :: Int -> Expression -> Parser Expression
expression1 !minPrecedence lhs = maybe (pure lhs) (loop lhs) =<< peek
  where
    loop lhs' op
        | precedence op >= minPrecedence = do
            space
            symbol <- binaryOperator
            rhs <- nonBinary
            lookaheadMaybe <- peek
            case lookaheadMaybe of
                Nothing -> pure $ BinaryOp lhs' symbol rhs
                Just lookahead -> do
                    (rhs', lookaheadMaybe') <- loop' rhs op lookahead
                    let result = BinaryOp lhs' symbol rhs'
                    maybe (pure result) (loop result) lookaheadMaybe'
        | otherwise = pure lhs'

    loop' rhs op lookahead
        | (precedence lookahead > precedence op)
            || (precedence lookahead == precedence op && associativity lookahead == RightAssoc) = do
            rhs' <- expression1 (precedence lookahead) rhs
            space
            lookaheadMaybe <- peek
            maybe (pure (rhs', Nothing)) (loop' rhs' op) lookaheadMaybe
        | otherwise = pure (rhs, Just lookahead)

    peek = optional $ lookAhead $ binarySymbolToOperator <$> binaryOperator

nonBinary :: Parser Expression
nonBinary = do
    space
    lhs <- choice [unary, parenthesis, value]
    exprs <- many $
        try $ (space *> between (char '(') (space *> char ')') (expression0 `sepBy` char ','))
    space
    pure $ foldl' Call lhs exprs

unary :: Parser Expression
unary = UnaryOp <$> unaryOperator <*> nonBinary

integer :: Parser Integer
integer = Lexer.decimal <* notFollowedBy (char 'e' <|> char '.')

value :: Parser Expression
value = constant <|> variable

constant :: Parser Expression
constant = Value . Constant <$> choice
    [ L.Integer <$> try integer
    , L.Double  <$> Lexer.float
    , L.Char    <$> char''
    , L.Bool    <$> bool
    , L.Text    <$> text
    ]

variable :: Parser Expression
variable = Value . Variable <$> variableName

parenthesis :: Parser Expression
parenthesis = between (char '(') (char ')') (Parenthesis <$> expression0)

bool :: Parser Bool
bool = do
    b <- string "false" <|> string "true"
    case b of
        "false" -> pure False
        "true" -> pure True
        _ -> fail $
            "Could not read '"
            <> toString b
            <> "'. Perhaps you meant 'false' or 'true'?"

char'' :: Parser Char
char'' = between (char '\'') (char '\'') Lexer.charLiteral

text :: Parser Text
text = toText <$> (char '"' *> manyTill Lexer.charLiteral (char '"'))

variableName :: Parser Text
variableName = do
    head' <- letterChar <|> char '_'
    tail' <- takeWhileP Nothing (\c -> isAlphaNum c || c == '_')
    pure $ Text.cons head' tail'

errorUnaryNotFound :: (IsString s, Semigroup s) => s -> s
errorUnaryNotFound symbol' =
    "Undefined unary operator '"
    <> symbol'
    <> "'. This error should not appear and is likely a bug. "
    <> "Please report it."

errorBinaryNotFound :: (IsString s, Semigroup s) => s -> s
errorBinaryNotFound symbol' =
    "Undefined binary operator '"
    <> symbol'
    <> "'. This error should not appear and is likely a bug. "
    <> "Please report it."

unaryOperator :: Parser UnarySymbol
unaryOperator = do
    symbol' <- choice (string <$> Map.elems unarySymbolToText)
    maybe (fail $ errorUnaryNotFound $ toString symbol') pure $ Map.lookup symbol' unaryTextToSymbol

binaryOperator :: Parser BinarySymbol
binaryOperator = do
    symbol' <- choice (string <$> Map.elems binarySymbolToText)
    maybe (fail $ errorBinaryNotFound $ toString symbol') pure $ Map.lookup symbol' binaryTextToSymbol

biject :: (Ord v) => Map k v -> Map v k
biject = Map.fromList . fmap swap . Map.toList

unarySymbolToText :: Map UnarySymbol Text
unarySymbolToText = Map.fromList
    [ (Negate, "-")
    , (Not   , "!")
    ]

unaryTextToSymbol :: Map Text UnarySymbol
unaryTextToSymbol = biject unarySymbolToText

binarySymbolToText :: Map BinarySymbol Text
binarySymbolToText = Map.fromList
    [ (Add         , "+"  )
    , (Divide      , "/"  )
    , (Multiply    , "*"  )
    , (Subtract    , "-"  )
    , (Different   , "<>" )
    , (Equal       , "="  )
    , (Greater     , ">"  )
    , (GreaterEqual, ">=" )
    , (Less        , "<"  )
    , (LessEqual   , "<=" )
    , (And         , "and")
    , (Or          , "or" )
    ]

binaryTextToSymbol :: Map Text BinarySymbol
binaryTextToSymbol = biject binarySymbolToText
