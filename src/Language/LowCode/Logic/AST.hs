module Language.LowCode.Logic.AST
    ( module Language.Common
    , Field (..)
    , Constructor (..)
    , Environment (..)
    , Metadata (..)
    , AST (..)
    , Expression (..)
    , codegenE
    , Literal (..)
    , codegenL
    , Type (..)
    , boolType
    , unitType
    , parseExpression
    , unaryToText
    , mapUnaryToText
    , mapTextToUnary
    , binaryToText
    , mapBinaryToText
    , mapTextToBinary
    ) where

import           Universum hiding (Type, bool, bracket, many, take, takeWhile, try)
import qualified Universum.Unsafe as Unsafe

import           Data.Aeson hiding (Array, Bool)
import           Data.Aeson.Types (prependFailure, unexpected)
import qualified Data.Char as Char
import           Data.Default.Class
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import           Text.Megaparsec hiding (unexpected)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

import           Language.Codegen
import           Language.Common
import           Language.Emit

data Field = Field
    { fieldName :: Name
    , fieldType :: Type
    } deriving (Eq, Generic, Ord, Show, FromJSON, ToJSON)

data Constructor = Constructor
    { constructorName  :: Name
    , constructorTypes :: [Type]
    } deriving (Eq, Generic, Ord, Show, FromJSON, ToJSON)

data Environment = Environment
    { externs         :: Map Name Type
    , recordTemplates :: Map Name [Field]
    , adtTemplates    :: Map Name [Constructor]
    } deriving (Eq, Generic, Show, FromJSON, ToJSON)

instance Default Environment where
    def = Environment Map.empty Map.empty Map.empty

newtype Metadata = Metadata
    { position :: Double2
    } deriving (Eq, Generic, Show, FromJSON, ToJSON)

data AST metadata
    -- | Assigns a new value to the first expression, and what follows after.
    = Assign metadata Expression Expression (AST metadata)
    -- | Represents the end of a cycle.
    | End
    -- | Executes a raw expression. Useful for Call.
    | Expression metadata Expression (AST metadata)
    -- | Allows a condition to be tested, and contains the true branch, false
    -- branch and what follows after the branches.
    | If metadata Expression (AST metadata) (AST metadata) (AST metadata)
    -- | Exits the function, optionally returning a value.
    | Return metadata (Maybe Expression)
    -- | Represents the start of a cycle with a given name and a list of
    -- parameters.
    | Start metadata Name !Type [Name] (AST metadata)
    -- | Declares a variable with the specified name and type, followed by the
    -- remainder of the cycle.
    | Var metadata Name !Type Expression (AST metadata)
    -- | Represents a condition which should be run while a predicate is not
    -- met, followed by the remainder of the cycle.
    | While metadata Expression (AST metadata) (AST metadata)
    deriving (Eq, Functor, Show)

instance (FromJSON metadata) => FromJSON (AST metadata) where
    parseJSON = withObject "Language.LowCode.Logic.AST.AST" \o -> o .: "tag" >>= \case
        "assign"      -> Assign     <$> o .:  "metadata"
                                    <*> o .:  "leftExpression"
                                    <*> o .:  "rightExpression"
                                    <*> o .:? "nextAst"        .!= End
        "expression"  -> Expression <$> o .:  "metadata"
                                    <*> o .:  "expression"
                                    <*> o .:? "nextAst"        .!= End
        "if"          -> If         <$> o .:  "metadata"
                                    <*> o .:  "expression"
                                    <*> o .:  "trueBranchAst"
                                    <*> o .:? "falseBranchAst" .!= End
                                    <*> o .:? "nextAst"        .!= End
        "return"      -> Return     <$> o .:  "metadata"
                                    <*> o .:? "expression"
        "start"       -> Start      <$> o .:  "metadata"
                                    <*> o .:  "name"
                                    <*> o .:  "returnType"
                                    <*> o .:  "arguments"
                                    <*> o .:? "nextAst"        .!= End
        "var"         -> Var        <$> o .:  "metadata"
                                    <*> o .:  "name"
                                    <*> o .:  "type"
                                    <*> o .:  "expression"
                                    <*> o .:? "nextAst"        .!= End
        "while"       -> While      <$> o .:  "metadata"
                                    <*> o .:  "expression"
                                    <*> o .:  "whileAst"
                                    <*> o .:? "nextAst"        .!= End
        other         -> fail $
                            "Unknown tag '" <> other <> "'. Available tags are:\
                            \ 'assign', 'expression', 'if', 'return', 'start',\
                            \ 'var' and 'while'."

instance (ToJSON metadata) => ToJSON (AST metadata) where
    toJSON = \case
        Assign metadata left right ast -> object
            [ "metadata"         .= metadata
            , "tag"              .= String "assign"
            , "leftExpression"   .= left
            , "rightExpression"  .= right
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
    = Access Expression Name
    | BinaryOp Expression !BinarySymbol Expression
    | Call Expression [Expression]
    | Index Expression Expression
    | Literal Literal
    | Parenthesis Expression
    | UnaryOp !UnarySymbol Expression
    | Variable Name
    deriving (Eq, Show)

instance FromJSON Expression where
    parseJSON (String s) = either (fail . toString) pure $ parseExpression s
    parseJSON (Object o) = o .: "tag" >>= \case
        "access"      -> Access      <$> o .: "expression"
                                     <*> o .: "name"
        "binaryOp"    -> BinaryOp    <$> o .: "leftExpression"
                                     <*> o .: "symbol"
                                     <*> o .: "rightExpression"
        "call"        -> Call        <$> o .: "expression"
                                     <*> o .: "arguments"
        "index"       -> Index       <$> o .: "leftExpression"
                                     <*> o .: "rightExpression"
        "literal"     -> Literal     <$> o .: "value"
        "parenthesis" -> Parenthesis <$> o .: "expression"
        "unaryOp"     -> UnaryOp     <$> o .: "symbol"
                                     <*> o .: "expression"
        "variable"    -> Variable    <$> o .: "name"
        other         -> fail $
                            "Expected 'access', 'binaryOp', 'call', 'index', 'literal',\
                            \ 'parenthesis', 'unaryOp' or 'variable', but got '"
                            <> other <> "'."
    parseJSON other = prependFailure "Expected String or Object, but got " (unexpected other)

instance ToJSON Expression where
    toJSON = \case
        Access expression name -> object
            [ "tag"             .= String "access"
            , "expression"      .= expression
            , "name"            .= name
            ]
        BinaryOp left symbol' right -> object
            [ "tag"             .= String "binaryOp"
            , "leftExpression"  .= left
            , "symbol"          .= symbol'
            , "rightExpression" .= right
            ]
        Call expression arguments -> object
            [ "tag"             .= String "call"
            , "expression"      .= expression
            , "arguments"       .= arguments
            ]
        Index left right -> object
            [ "tag"             .= String "index"
            , "leftExpression"  .= left
            , "rightExpression" .= right
            ]
        Literal value' -> object
            [ "tag"             .= String "literal"
            , "value"           .= value'
            ]
        Parenthesis expression -> object
            [ "tag"             .= String "parenthesis"
            , "expression"      .= expression
            ]
        UnaryOp symbol' expression -> object
            [ "tag"             .= String "unaryOp"
            , "symbol"          .= symbol'
            , "expression"      .= expression
            ]
        Variable name -> object
            [ "tag"             .= String "variable"
            , "name"            .= name
            ]

-- Codegen for Expression never fails, so this function is safe.
codegenE :: Expression -> Text
codegenE = Unsafe.fromJust . rightToMaybe . evalCodegenT () . codegen

instance Codegen Expression where
    type GeneratorState Expression = ()

    codegen = \case
        Access expr name -> mconcat <$> sequence
            [ codegen expr
            , emitM "."
            , emitM name
            ]
        BinaryOp left symbol' right -> mconcat <$> sequence
            [ codegen left
            , emitM " "
            , emitM (binaryToText symbol')
            , emitM " "
            , codegen right
            ]
        Call expr exprs -> mconcat <$> sequence
            [ codegen expr
            , emitBetween' "(" ")" $ exprs `separatedBy'` ", "
            ]
        Index expr inner -> mconcat <$> sequence
            [ codegen expr
            , emitBetween' "[" "]" $ codegen inner
            ]
        Literal value' -> codegen value'
        Parenthesis expr -> emitBetween' "(" ")" $ codegen expr
        UnaryOp symbol' expr -> mconcat <$> sequence
            [ emitM $ unaryToText symbol'
            , codegen expr
            ]
        Variable name' -> emitM name'

-- TODO: Add let ... in ... so that we can declare variables and functions?
data Literal
    = Algebraic Name [Expression]
    | Array [Expression]
    | Char Char
    | Double Double
    | Integer Integer
    | Record Name [(Name, Expression)]
    | Text Text
    deriving (Eq, Show)

-- Codegen for Literal never fails, so this function is safe.
codegenL :: Literal -> Text
codegenL = Unsafe.fromJust . rightToMaybe . evalCodegenT () . codegen

instance Codegen Literal where
    type GeneratorState Literal = ()

    codegen = \case
        Algebraic name fields -> codegenAlgebraic name fields
        Array a -> emitBetween' "[" "]" $ a `separatedBy'` ", "
        Char c -> emitBetween' "'" "'" $ emitM (Text.singleton c)
        Double d -> emitM $ show d
        Integer i -> emitM $ show i
        Record r fs -> codegenRecord r fs
        Text t -> emitBetween' "\""  "\"" $ emitM t
      where
        codegenAlgebraic name fields = mconcat <$> sequence
            [ emitM name
            , emitBetween' "(" ")" $ fields `separatedBy'` ", "
            ]

        codegenField fieldName expr = mconcat <$> sequence
            [ emitM fieldName
            , emitM " = "
            , codegen expr
            ]

        codegenRecord recordName fields = mconcat <$> sequence
            [ emitM recordName
            , emitBetween' "{" "}" $ separatedByF (uncurry codegenField) (emit ", ") fields
            ]

instance FromJSON Literal where
    parseJSON = withObject "Language.LowCode.Logic.AST.Variable" \o -> o .: "type" >>= \case
        "adt"    -> Algebraic <$> o .: "name"  <*> o .: "fields"
        "record" -> Record    <$> o .: "name"  <*> o .: "fields"
        "array"  -> Array     <$> o .: "value"
        "char"   -> Char      <$> o .: "value"
        "double" -> Double    <$> o .: "value"
        "integer"-> Integer   <$> o .: "value"
        "text"   -> Text      <$> o .: "value"
        other    -> fail $
            "Expected 'algebraic', 'array', 'char', 'double', 'function',\
            \ 'integer', 'record' or 'text', but got '" <> other <> "'."

instance ToJSON Literal where
    toJSON = \case
        Algebraic name constructors -> object
            [ "type"   .= String "adt"
            , "name"   .= name
            , "fields" .= constructors
            ]
        Array a -> object
            [ "type"   .= String "array"
            , "value"  .= a
            ]
        Char c -> object
            [ "type"   .= String "char"
            , "value"  .= c
            ]
        Double d -> object
            [ "type"   .= String "double"
            , "value"  .= d
            ]
        Integer i -> object
            [ "type"   .= String "integer"
            , "value"  .= i
            ]
        Record recordName fields -> object
            [ "type"   .= String "record"
            , "name"   .= recordName
            , "fields" .= fields
            ]
        Text t -> object
            [ "type"   .= String "text"
            , "value"  .= t
            ]

data Type
    = AlgebraicType Name [Constructor]
    | ArrayType Type
    | CharType
    | DoubleType
    | FunctionType [Type] Type
    | IntegerType
    | RecordType [Field]
    | TextType
    deriving (Eq, Ord, Show)

-- TODO: In the future, move these "concrete" types to a standard library.
boolType :: Type
boolType = AlgebraicType "Bool" [Constructor "False" [], Constructor "True" []]

unitType :: Type
unitType = AlgebraicType "Unit" [Constructor "Unit" []]

instance FromJSON Type where
    parseJSON = withObject "Language.LowCode.Logic.AST.VariableType" \o -> o .: "type" >>= \case
        "adt"      -> AlgebraicType <$> o .: "name" <*> o .: "constructors"
        "array"    -> ArrayType <$> o .: "elements"
        "char"     -> pure CharType
        "double"   -> pure DoubleType
        "function" -> FunctionType <$> o .: "arguments" <*> o .: "return"
        "integer"  -> pure IntegerType
        "record"   -> RecordType <$> o .: "fields"
        "text"     -> pure TextType
        other      -> fail $
                       "Expected 'array', 'char', 'double', 'function',\
                       \ 'integer', 'record' or 'text',\
                       \ but got '" <> other <> "'."

instance ToJSON Type where
    toJSON = \case
        AlgebraicType name constructors -> object
            [ "type"         .= String "adt"
            , "name"         .= name
            , "constructors" .= constructors
            ]
        ArrayType elements -> object
            [ "type"         .= String "array"
            , "elements"     .= elements
            ]
        CharType    -> object [ "type" .= String "char"    ]
        DoubleType  -> object [ "type" .= String "double"  ]
        FunctionType arguments return' -> object
            [ "type"          .= String "function"
            , "arguments"     .= arguments
            , "return"        .= return'
            ]
        IntegerType -> object [ "type" .= String "integer" ]
        RecordType fieldsType -> object
            [ "type"        .= String "record"
            , "fields"      .= fieldsType
            ]
        TextType    -> object [ "type" .= String "text"    ]

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
expression0 = expression1 0 =<< primary

expression1 :: Int -> Expression -> Parser Expression
expression1 !minPrecedence lhs = maybe (pure lhs) (loop lhs) =<< peek
  where
    loop lhs' op
        | precedence op >= minPrecedence = do
            space
            symbol <- binaryOperator
            rhs <- primary
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

primary :: Parser Expression
primary = space *> (secondary =<< choice [value, unary, parenthesis]) <* space

secondary :: Expression -> Parser Expression
secondary lhs = do
    space
    rhs <- optional (secondary =<< choice [access' lhs, call' lhs, index' lhs])
    pure $ fromMaybe lhs rhs
  where
    access' lhs' = Access lhs' <$> access
    call' lhs' = Call lhs' <$> tuple
    index' lhs' = Index lhs' <$> index

access :: Parser Name
access = char '.' *> space *> variableName

bracket :: Char -> Char -> Parser a -> Parser a
bracket left right = between (char left) (space *> char right)

tuple :: Parser [Expression]
tuple = bracket '(' ')' (expression0 `sepBy` char ',')

index :: Parser Expression
index = bracket '[' ']' expression0

array :: Parser [Expression]
array = bracket '[' ']' (expression0 `sepEndBy` char ',')

unary :: Parser Expression
unary = liftA2 UnaryOp unaryOperator primary

integer :: Parser Integer
integer = do
    radixMaybe <- optional (mkRadix <* char' 'r')
    let radix = fromMaybe 10 radixMaybe
    when (radix < 1 || radix > 36) $
        fail "Radix must be a natural number in [1, 36] range."
    number <- takeNums
    when (any ((>= radix) . toInt) number)
        if radixMaybe == Nothing
        then fail "Not a valid number."
        else fail "Number must contain only digits or ASCII letters and must be encodable in the given radix."
    pure $ mkNum radix number
  where
    mkRadix = mkNum 10 <$> takeWhile1P (Just "digit") Char.isDigit
    takeNums = takeWhile1P (Just "digit") (\c -> Char.isDigit c || Char.isAsciiLower c || Char.isAsciiUpper c)
    mkNum r = foldl' (\a c -> a * r + toInt c) 0
    toInt = toInteger . toInt'
    toInt' c
        | Char.isDigit      c = Char.digitToInt c
        | Char.isAsciiLower c = Char.ord c - Char.ord 'a' + 10
        | Char.isAsciiUpper c = Char.ord c - Char.ord 'A' + 10
        | otherwise           = error $ "Panic in integer: Unknown digit: " <> Text.singleton c

value :: Parser Expression
value = constant <|> variable

constant :: Parser Expression
constant = Literal <$> choice
    [ Array   <$> array
    , Char    <$> char''
    , Double  <$> try Lexer.float
    , Integer <$> try integer
    , record' <$> try record
    , Text    <$> text
    ]
  where
    record' = uncurry Record

variable :: Parser Expression
variable = Variable <$> variableName

parenthesis :: Parser Expression
parenthesis = bracket '(' ')' (Parenthesis <$> expression0)

char'' :: Parser Char
char'' = between (char '\'') (char '\'') Lexer.charLiteral

text :: Parser Text
text = toText <$> (char '"' *> manyTill Lexer.charLiteral (char '"'))

variableName :: Parser Text
variableName = do
    head' <- letterChar <|> char '_'
    tail' <- takeWhileP Nothing (\c -> Char.isAlphaNum c || c == '_')
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

errorUnaryNotFound :: (IsString s, Semigroup s) => s -> s
errorUnaryNotFound symbol' = "Undefined unary operator '" <> symbol' <> "'."

errorBinaryNotFound :: (IsString s, Semigroup s) => s -> s
errorBinaryNotFound symbol' = "Undefined binary operator '" <> symbol' <> "'."

isOperatorSymbol :: Char -> Bool
isOperatorSymbol c = (c `notElem` forbidden) && (Char.isPunctuation c || Char.isSymbol c)
  where
    forbidden :: Text
    forbidden = ",.()[]{}"

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
