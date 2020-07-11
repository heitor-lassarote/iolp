module Language.LowCode.Logic.AST
    ( Environment (..)
    , Metadata (..)
    , AST (..)
    , Expression (..)
    , codegenE
    , Variable (..)
    , VariableType (..)
    , parseExpression
    , unaryToText
    , mapUnaryToText
    , mapTextToUnary
    , binaryToText
    , mapBinaryToText
    , mapTextToBinary
    ) where

import           Universum hiding (bool, many, take, takeWhile, try)
import qualified Universum.Unsafe as Unsafe

import           Data.Aeson hiding (Array, Bool)
import           Data.Char (isAlphaNum, isPunctuation, isSymbol)
import           Data.Default.Class
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

instance Default Environment where
    def = Environment Map.empty

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
    toJSON = String . codegenE

-- Codegen for expression never fails, so this function is safe.
codegenE :: Expression -> Text
codegenE = Unsafe.fromJust . rightToMaybe . evalCodegenT () . codegen

instance Codegen Expression where
    type GeneratorState Expression = ()

    codegen = \case
        BinaryOp left symbol' right -> mconcat <$> sequence
            [ codegen left
            , emitM " "
            , emitM (binaryToText symbol')
            , emitM " "
            , codegen right
            ]
        Call expr exprs -> mconcat <$> sequence
            [ codegen expr
            , emitM "("
            , mconcat . fmap emit . intersperse ", " <$> traverse codegen exprs
            , emitM ")"
            ]
        Parenthesis expression -> mconcat <$> sequence
            [ emitM "("
            , codegen expression
            , emitM ")"
            ]
        UnaryOp symbol' expression -> mconcat <$> sequence
            [ emitM $ unaryToText symbol'
            , codegen expression
            ]
        Value value' -> case value' of
            Variable variable' -> emitM variable'
            Constant constant' -> case constant' of
                Array a -> codegenArray a
                Bool b -> emitM if b then "true" else "false"
                Char c -> pure $ emit "'" <> emit (Text.singleton c) <> emit "'"
                Double d -> emitM $ show d
                Integer i -> emitM $ show i
                Record r fs -> codegenRecord r fs
                Text t -> pure $ emit "\"" <> emit t <> emit "\""
                Unit -> emitM "()"
      where
        codegenArray exprs = mconcat <$> sequence
            [ emitM "["
            , mconcat . fmap emit . intersperse ", " <$> traverse codegen exprs
            , emitM "]"
            ]

        codegenField fieldName expr = mconcat <$> sequence
            [ emitM fieldName
            , emitM " = "
            , codegen expr
            ]

        codegenRecord recordName fields = mconcat <$> sequence
            [ emitM recordName
            , emitM " { "
            , mconcat . fmap emit . intersperse ", " <$> traverse (uncurry codegenField) fields
            , emitM " }"
            ]

-- TODO: Add let ... in ... so that we can declare variables and functions?
data Variable
    = Array [Expression]
    | Bool Bool
    | Char Char
    | Double Double
    | Integer Integer
    | Record Name [(Name, Expression)]
    | Text Text
    | Unit
    deriving (Eq, Show)

instance FromJSON Variable where
    parseJSON = withObject "Language.LowCode.Logic.AST.Variable" \o -> do
        tag <- o .: "type"
        -- TODO: Should unit be encoded as ()?
        if tag == "unit"
        then pure Unit
        else if tag == "record"
        then Record <$> o .: "name" <*> o .: "fields"
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
                               <> ", 'record', 'text' or 'unit', but got '" <> other <> "'."

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
    toJSON (Record recordName fields) = object
        [ "type"   .= String "field"
        , "name"   .= String recordName
        , "fields" .= toJSON fields
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
    | RecordType [VariableType]
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
        [ "type"      .= String "function"
        , "arguments" .= arguments
        , "return"    .= return'
        ]
    toJSON IntegerType = object [ "type" .= String "integer" ]
    toJSON (RecordType fieldsType) = object
        [ "type"   .= String "record"
        , "fields" .= fieldsType
        ]
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
    lhs <- choice [value, unary, parenthesis]
    exprs <- many $
        try (space *> between (char '(') (space *> char ')') (expression0 `sepBy` char ','))
    space
    pure $ foldl' Call lhs exprs

array :: Parser [Expression]
array = between (char '[') (space *> char ']') (expression0 `sepEndBy` char ',')

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
    , record' <$> try record
    , Text    <$> text
    , unit'   <$> unit
    ]
  where
    record' = uncurry Record
    unit' = const Unit

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

record :: Parser (Name, [(Name, Expression)])
record = do
    recordName <- variableName
    space
    fields <- between (char '{') (char '}') $ flip sepBy (char ',') do
        space
        fieldName <- variableName
        space
        void $ char '='
        space
        expr <- expression0
        space
        pure (fieldName, expr)
    space
    pure (recordName, fields)

unit :: Parser ()
unit = void $ string "()"

errorUnaryNotFound :: (IsString s, Semigroup s) => s -> s
errorUnaryNotFound symbol' = "Undefined unary operator '" <> symbol' <> "'."

errorBinaryNotFound :: (IsString s, Semigroup s) => s -> s
errorBinaryNotFound symbol' = "Undefined binary operator '" <> symbol' <> "'."

isOperatorSymbol :: Char -> Bool
isOperatorSymbol c = (c `notElem` forbidden) && (isPunctuation c || isSymbol c)
  where
    forbidden :: String
    forbidden = ",()[]{}"

unaryOperator :: Parser UnarySymbol
unaryOperator = do
    symbol' <- takeWhile1P (Just "unary symbol") isOperatorSymbol
    maybe (fail $ errorUnaryNotFound $ toString symbol') pure $ Map.lookup symbol' mapTextToUnary

binaryOperator :: Parser BinarySymbol
binaryOperator = do
    symbol' <- takeWhile1P (Just "binary symbol") isOperatorSymbol
    maybe (fail $ errorBinaryNotFound $ toString symbol') pure $ Map.lookup symbol' mapTextToBinary

biject :: (Ord v) => Map k v -> Map v k
biject = Map.fromList . fmap swap . Map.toList

unaryToText :: UnarySymbol -> Text
unaryToText = \case
    Negate -> "-"
    Not    -> "!"

mapTextToUnary :: Map Text UnarySymbol
mapTextToUnary = biject mapUnaryToText

mapUnaryToText :: Map UnarySymbol Text
mapUnaryToText = Map.fromDistinctAscList $ map (id &&& unaryToText) [Negate .. Not]

binaryToText :: BinarySymbol -> Text
binaryToText = \case
    Add          ->  "+"
    Divide       ->  "/"
    Multiply     ->  "*"
    Subtract     ->  "-"
    Different    ->  "<>"
    Equal        ->  "="
    Greater      ->  ">"
    GreaterEqual ->  ">="
    Less         ->  "<"
    LessEqual    ->  "<="
    And          ->  "and"
    Or           ->  "or"

mapTextToBinary :: Map Text BinarySymbol
mapTextToBinary = biject mapBinaryToText

mapBinaryToText :: Map BinarySymbol Text
mapBinaryToText = Map.fromDistinctAscList $ map (id &&& binaryToText) [Add .. Or]
