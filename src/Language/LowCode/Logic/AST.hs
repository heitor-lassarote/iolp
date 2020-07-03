module Language.LowCode.Logic.AST
    ( Environment (..)
    , Metadata (..)
    , AST (..)
    , Expression (..)
    , Variable (..)
    , VariableType (..)
    , parseExpression
    ) where

import           Universum hiding (bool, many, take, takeWhile, try)
import qualified Universum.Unsafe as Unsafe

import           Control.Monad.Trans.Except (throwE)
import           Data.Aeson hiding (Array, Bool)
import           Data.Char (isAlphaNum)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

import           Language.Codegen
import           Language.Common
import           Language.Emit

newtype Environment = Environment
    { externs :: Map Name VariableType
    } deriving (Eq, Generic, Show, FromJSON, ToJSON)

newtype Metadata = Metadata
    { position :: Double2
    } deriving (Eq, Generic, Show, FromJSON, ToJSON)

data AST
    -- | Assigns a new value to the variable with the specified name and type
    -- and what follows after.
    = Assign Metadata Name Expression AST
    -- | Represents the end of a cycle.
    | End
    -- | Executes a raw expression. Useful for Call.
    | Expression Metadata Expression AST
    -- | Allows a condition to be tested, and contains the true branch, false
    -- branch and what follows after the branches.
    | If Metadata Expression AST AST AST
    -- | Exits the function, optionally returning a value.
    | Return Metadata (Maybe Expression)
    -- | Represents the start of a cycle with a given name and a list of
    -- parameters.
    | Start Metadata Name !VariableType [Name] AST
    -- | Declares a variable with the specified name and type, followed by the
    -- remainder of the cycle.
    | Var Metadata Name !VariableType Expression AST
    -- | Represents a condition which should be run while a predicate is not
    -- met, followed by the remainder of the cycle.
    | While Metadata Expression AST AST
    deriving (Eq, Show)

instance FromJSON AST where
    parseJSON = withObject "Language.LowCode.Logic.AST.AST" \o -> o .: "tag" >>= \case
        "assign"      -> Assign     <$> o .:  "metadata"
                                    <*> o .:  "name"
                                    <*> o .:  "expression"
                                    <*> o .:? "nextAst"         .!= End
        "expression"  -> Expression <$> o .:  "metadata"
                                    <*> o .:  "expression"
                                    <*> o .:? "nextAst"         .!= End
        "if"          -> If         <$> o .:  "metadata"
                                    <*> o .:  "expression"
                                    <*> o .:  "trueBranchAst"
                                    <*> o .:? "falseBranchAst"  .!= End
                                    <*> o .:? "nextAst"         .!= End
        "return"      -> Return     <$> o .:  "metadata"
                                    <*> o .:? "expression"
        "start"       -> Start      <$> o .:  "metadata"
                                    <*> o .:  "name"
                                    <*> o .:  "returnType"
                                    <*> o .:  "arguments"
                                    <*> o .:? "nextAst"         .!= End
        "var"         -> Var        <$> o .:  "metadata"
                                    <*> o .:  "name"
                                    <*> o .:  "type"
                                    <*> o .:  "expression"
                                    <*> o .:? "nextAst"         .!= End
        "while"       -> While      <$> o .:  "metadata"
                                    <*> o .:  "expression"
                                    <*> o .:  "whileAst"
                                    <*> o .:? "nextAst"         .!= End
        other         -> fail $ "Unknown tag '"
                            <> other
                            <> "'. Available tags are: 'assign', 'expression'"
                            <> ", 'if', 'return', 'start', 'var' and 'while'."

instance ToJSON AST where
    toJSON = \case
        Assign metadata name expression ast -> object
            [ "metadata"         .= metadata
            , "tag"              .= String "assign"
            , "name"             .= String name
            , "expression"       .= expression
            , "nextAst"          .= ast
            ]
        End -> Null
        Expression metadata expression ast -> object
            [ "metadata"         .= metadata
            , "tag"              .= String "expression"
            , "expression"       .= expression
            , "nextAst"          .= ast
            ]
        If metadata expression true false ast -> object
            [ "metadata"         .= metadata
            , "tag"              .= String "if"
            , "expression"       .= expression
            , "trueBranchAst"    .= true
            , "falseBranchAst"   .= false
            , "nextAst"          .= ast
            ]
        Return metadata expression -> object
            [ "metadata"         .= metadata
            , "tag"              .= String "return"
            , "expression"       .= expression
            ]
        Start metadata name returnType arguments ast -> object
            [ "metadata"         .= metadata
            , "tag"              .= String "start"
            , "name"             .= String name
            , "returnType"       .= returnType
            , "arguments"        .= arguments
            , "nextAst"          .= ast
            ]
        Var metadata name type' expression ast -> object
            [ "metadata"         .= metadata
            , "tag"              .= String "var"
            , "name"             .= String name
            , "type"             .= type'
            , "expression"       .= expression
            , "nextAst"          .= ast
            ]
        While metadata expression body ast -> object
            [ "metadata"         .= metadata
            , "tag"              .= String "while"
            , "expression"       .= expression
            , "whileAst"         .= body
            , "nextAst"          .= ast
            ]

data Expression
    = BinaryOp Expression !BinarySymbol Expression
    | Call Expression [Expression]
    | Parenthesis Expression
    | UnaryOp !UnarySymbol Expression
    | Value (ValueType Variable)
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
                Array a -> emit $ show a
                Bool b -> emit $ show b
                Char c -> emit "'" <> emit (Text.singleton c) <> emit "'"
                Double d -> emit $ show d
                Integer i -> emit $ show i
                Text t -> emit "\"" <> emit t <> emit "\""
                Unit -> emit "()"
      where
        tryUnaryText sym =
            maybe (lift $ throwE $ errorUnaryNotFound $ show sym) emitM $
                Map.lookup sym unarySymbolToText
        tryBinaryText sym =
            maybe (lift $ throwE $ errorBinaryNotFound $ show sym) emitM $
                Map.lookup sym binarySymbolToText

-- TODO: Add let ... in ... so that we can declare variables and functions?
data Variable
    = Array [Expression]
    | Bool Bool
    | Char Char
    | Double Double
    | Integer Integer
--    | Polymorphic
    | Text Text
    | Unit
    deriving (Eq, Show)

instance FromJSON Variable where
    parseJSON = withObject "Language.LowCode.Logic.AST.Variable" \o -> do
        tag <- o .: "type"
        -- TODO: Should unit be encoded as ()?
        if tag == "unit"
        then pure Unit
--        else if tag == "polymorphic"
--        then pure Polymorphic
        else do
            value' <- o .: "value"
            case tag of
                "array"   -> Array   <$> parseJSON value'
                "bool"    -> Bool    <$> parseJSON value'
                "char"    -> Char    <$> parseJSON value'
                "double"  -> Double  <$> parseJSON value'
                "integer" -> Integer <$> parseJSON value'
                "text"    -> Text    <$> parseJSON value'
                other     -> fail $
                               "Expected 'array', 'bool', 'char', 'double', 'function', 'integer'"
                               <> ", 'polymorphic', 'text' or 'unit', but got '" <> other <> "'."

instance ToJSON Variable where
    toJSON (Array a) = object
        [ "type"  .= String "array"
        , "value" .= a
        ]
    toJSON (Bool b) = object
        [ "type"  .= String "bool"
        , "value" .= b
        ]
    toJSON (Char c) = object
        [ "type"  .= String "char"
        , "value" .= c
        ]
    toJSON (Double d) = object
        [ "type"  .= String "double"
        , "value" .= d
        ]
    toJSON (Integer i) = object
        [ "type"  .= String "integer"
        , "value" .= i
        ]
    toJSON (Text t) = object
        [ "type"  .= String "text"
        , "value" .= t
        ]
    toJSON Unit = object
        [ "type"  .= String "unit"
        ]

data VariableType
    = ArrayType VariableType
    | BoolType
    | CharType
    | DoubleType
    | FunctionType [VariableType] VariableType
    | IntegerType
    | TextType
    | UnitType
    deriving (Eq, Ord, Show)

instance FromJSON VariableType where
    parseJSON = withObject "Language.LowCode.Logic.AST.VariableType" \o -> o .: "type" >>= \case
        "array"    -> ArrayType <$> o .: "elements"
        "bool"     -> pure BoolType
        "char"     -> pure CharType
        "double"   -> pure DoubleType
        "function" -> FunctionType <$> o .: "arguments" <*> o .: "return"
        "integer"  -> pure IntegerType
        "text"     -> pure TextType
        "unit"     -> pure UnitType
        other      -> fail $
                       "Expected 'array', 'bool', 'char', 'double', 'function', 'integer', 'text' or"
                       <> " 'unit', but got '" <> other <> "'."

instance ToJSON VariableType where
    toJSON (ArrayType elements) = object
        [ "type"     .= String "array"
        , "elements" .= elements
        ]
    toJSON BoolType    = object [ "type" .= String "bool"    ]
    toJSON CharType    = object [ "type" .= String "char"    ]
    toJSON DoubleType  = object [ "type" .= String "double"  ]
    toJSON (FunctionType arguments return') = object
        -- TODO: Should it have a type such as (Int -> Bool) -> [Int] -> Bool?
        [ "type"      .= String "function"
        , "arguments" .= arguments
        , "return"    .= return'
        ]
    toJSON IntegerType = object [ "type" .= String "integer" ]
    toJSON TextType    = object [ "type" .= String "text"    ]
    toJSON UnitType    = object [ "type" .= String "unit"    ]

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
        try (space *> between (char '(') (space *> char ')') (expression0 `sepBy` char ','))
    space
    pure $ foldl' Call lhs exprs

array :: Parser [Expression]
array = between (char '[') (space *> char ']') (expression0 `sepBy` char ',')

unary :: Parser Expression
unary = liftA2 UnaryOp unaryOperator nonBinary

integer :: Parser Integer
integer = try (Lexer.decimal <* notFollowedBy (char 'e' <|> char '.'))

value :: Parser Expression
value = constant <|> variable

constant :: Parser Expression
constant = Value . Constant <$> choice
    [ Array   <$> array
    , Bool    <$> bool
    , Char    <$> char''
    , Double  <$> try Lexer.float
    , Integer <$> integer
    , Text    <$> text
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
