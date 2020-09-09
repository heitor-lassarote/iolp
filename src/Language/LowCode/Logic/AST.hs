module Language.LowCode.Logic.AST
    ( module Language.Common
    , HasMetadata (..)
    , Field (..)
    , Constructor (..)
    , Environment (..)
    , Metadata (..)
    , TopLevel (..)
    , AST (..)
    , Expression (..)
    , Literal (..)
    , Structure (..)
    , unit
    , Type (..)
    , parseExpression
    , boolType
    , unitType
    , unaryToText
    , mapUnaryToText
    , mapTextToUnary
    , binaryToText
    , mapBinaryToText
    , mapTextToBinary
    ) where

import           Universum hiding (Type, bool, bracket, many, take, takeWhile, try)

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
import           Utility (biject)

class HasMetadata f where
    getMetadata  :: f a -> a

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

data TopLevel expressionMetadata metadata
    -- | Represents the start of a cycle with a given name and a list of
    -- parameters.
    = Start metadata Name !Type [Name] (AST expressionMetadata metadata)

instance (FromJSON metadata) => FromJSON (TopLevel () metadata) where
    parseJSON = withObject "Language.LowCode.Logic.AST.TopLevel" \o -> o .: "tag" >>= \case
        "start" -> Start <$> o .: "metadata"
                         <*> o .: "name"
                         <*> o .: "returnType"
                         <*> o .: "arguments"
                         <*> o .: "nextAst"
        other   -> fail $ "Unknown tag '" <> other <> "'. Available tags are: 'start'."

instance (ToJSON expressionMetadata, ToJSON metadata) => ToJSON (TopLevel expressionMetadata metadata) where
    toJSON = \case
        Start metadata name returnType arguments ast -> object
            [ "metadata"   .= metadata
            , "tag"        .= String "start"
            , "name"       .= String name
            , "returnType" .= returnType
            , "arguments"  .= arguments
            , "nextAst"    .= ast
            ]

data AST expressionMetadata metadata
    -- | Assigns a new value to the first expression, and what follows after.
    = Assign metadata (Expression expressionMetadata) (Expression expressionMetadata) (AST expressionMetadata metadata)
    -- | Represents the end of a cycle.
    | End metadata
    -- | Executes a raw expression. Useful for Call.
    | Expression metadata (Expression expressionMetadata) (AST expressionMetadata metadata)
    -- | Allows a condition to be tested, and contains the true branch, false
    -- branch and what follows after the branches.
    | If metadata (Expression expressionMetadata) (AST expressionMetadata metadata) (AST expressionMetadata metadata) (AST expressionMetadata metadata)
    -- | Exits the function, optionally returning a value.
    | Return metadata (Maybe (Expression expressionMetadata))
    -- | Declares a variable with the specified name and type, followed by the
    -- remainder of the cycle.
    | Var metadata Name !Type (Expression expressionMetadata) (AST expressionMetadata metadata)
    -- | Represents a condition which should be run while a predicate is not
    -- met, followed by the remainder of the cycle.
    | While metadata (Expression expressionMetadata) (AST expressionMetadata metadata) (AST expressionMetadata metadata)
    deriving (Eq, Functor, Show)

instance HasMetadata (AST expressionMetadata) where
    getMetadata = \case
        Assign m _ _ _ -> m
        End m -> m
        Expression m _ _ -> m
        If m _ _ _ _ -> m
        Return m _ -> m
        Var m _ _ _ _ -> m
        While m _ _ _ -> m

instance (FromJSON metadata) => FromJSON (AST () metadata) where
    parseJSON = withObject "Language.LowCode.Logic.AST.AST" \o -> o .: "tag" >>= \case
        "assign"      -> Assign     <$> o .: "metadata"
                                    <*> o .: "leftExpression"
                                    <*> o .: "rightExpression"
                                    <*> o .: "nextAst"
        "end"         -> End        <$> o .: "metadata"
        "expression"  -> Expression <$> o .: "metadata"
                                    <*> o .: "expression"
                                    <*> o .: "nextAst"
        "if"          -> If         <$> o .: "metadata"
                                    <*> o .: "expression"
                                    <*> o .: "trueBranchAst"
                                    <*> o .: "falseBranchAst"
                                    <*> o .: "nextAst"
        "return"      -> Return     <$> o .: "metadata"
                                    <*> o .: "expression"
        "var"         -> Var        <$> o .: "metadata"
                                    <*> o .: "name"
                                    <*> o .: "type"
                                    <*> o .: "expression"
                                    <*> o .: "nextAst"
        "while"       -> While      <$> o .: "metadata"
                                    <*> o .: "expression"
                                    <*> o .: "whileAst"
                                    <*> o .: "nextAst"
        other         -> fail $
                            "Unknown tag '" <> other <> "'. Available tags are:\
                            \ 'assign', 'end', 'expression', 'if', 'return', 'start',\
                            \ 'var' and 'while'."

instance (ToJSON expressionMetadata, ToJSON metadata) => ToJSON (AST expressionMetadata metadata) where
    toJSON = \case
        Assign metadata left right ast -> object
            [ "metadata"         .= metadata
            , "tag"              .= String "assign"
            , "leftExpression"   .= left
            , "rightExpression"  .= right
            , "nextAst"          .= ast
            ]
        End metadata -> object
            [ "metadata"         .= metadata
            , "tag"              .= String "end"
            ]
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

instance HasMetadata Expression where
    getMetadata = \case
        Access m _ _ -> m
        BinaryOp m _ _ _ -> m
        Call m _ _ -> m
        Index m _ _ -> m
        Literal m _ -> m
        Parenthesis m _ -> m
        Structure m _ -> m
        UnaryOp m _ _ -> m
        Variable m _ -> m

data Expression metadata
    = Access metadata (Expression metadata) Name
    | BinaryOp metadata (Expression metadata) !BinarySymbol (Expression metadata)
    | Call metadata (Expression metadata) [(Expression metadata)]
    | Index metadata (Expression metadata) (Expression metadata)
    | Literal metadata Literal
    | Parenthesis metadata (Expression metadata)
    | Structure metadata (Structure metadata)
    | UnaryOp metadata !UnarySymbol (Expression metadata)
    | Variable metadata Name
    deriving (Eq, Functor, Show)

instance FromJSON (Expression ()) where
    parseJSON (String s) = either (fail . toString) pure $ parseExpression s
    parseJSON (Object o) = o .: "tag" >>= \case
        "access"      -> Access ()      <$> o .: "expression"
                                        <*> o .: "name"
        "binaryOp"    -> BinaryOp ()    <$> o .: "leftExpression"
                                        <*> o .: "symbol"
                                        <*> o .: "rightExpression"
        "call"        -> Call ()        <$> o .: "expression"
                                        <*> o .: "arguments"
        "index"       -> Index ()       <$> o .: "leftExpression"
                                        <*> o .: "rightExpression"
        "literal"     -> Literal ()     <$> o .: "value"
        "parenthesis" -> Parenthesis () <$> o .: "expression"
        "structure"   -> Structure ()   <$> o .: "structure"
        "unaryOp"     -> UnaryOp ()     <$> o .: "symbol"
                                        <*> o .: "expression"
        "variable"    -> Variable ()    <$> o .: "name"
        other         -> fail $
                            "Expected 'access', 'binaryOp', 'call', 'index', 'literal',\
                            \ 'parenthesis', 'structure', 'unaryOp' or 'variable', but got '"
                            <> other <> "'."
    parseJSON other = prependFailure "Expected String or Object, but got " (unexpected other)

instance ToJSON (Expression metadata) where
    toJSON = \case
        Access _ expression name -> object
            [ "tag"             .= String "access"
            , "expression"      .= expression
            , "name"            .= name
            ]
        BinaryOp _ left symbol' right -> object
            [ "tag"             .= String "binaryOp"
            , "leftExpression"  .= left
            , "symbol"          .= symbol'
            , "rightExpression" .= right
            ]
        Call _ expression arguments -> object
            [ "tag"             .= String "call"
            , "expression"      .= expression
            , "arguments"       .= arguments
            ]
        Index _ left right -> object
            [ "tag"             .= String "index"
            , "leftExpression"  .= left
            , "rightExpression" .= right
            ]
        Literal _ value' -> object
            [ "tag"             .= String "literal"
            , "value"           .= value'
            ]
        Parenthesis _ expression -> object
            [ "tag"             .= String "parenthesis"
            , "expression"      .= expression
            ]
        Structure _ structure' -> object
            [ "tag"             .= String "structure"
            , "structure"       .= structure'
            ]
        UnaryOp _ symbol' expression -> object
            [ "tag"             .= String "unaryOp"
            , "symbol"          .= symbol'
            , "expression"      .= expression
            ]
        Variable _ name -> object
            [ "tag"             .= String "variable"
            , "name"            .= name
            ]

instance Codegen (Expression metadata) where
    type GeneratorState (Expression metadata) = ()

    codegen = \case
        Access _ expr name -> mconcat <$> sequence
            [ codegen expr
            , emitM "."
            , emitM name
            ]
        BinaryOp _ left symbol' right -> mconcat <$> sequence
            [ codegen left
            , emitM " "
            , emitM (binaryToText symbol')
            , emitM " "
            , codegen right
            ]
        Call _ expr exprs -> mconcat <$> sequence
            [ codegen expr
            , emitBetween' "(" ")" $ exprs `separatedBy'` ", "
            ]
        Index _ expr inner -> mconcat <$> sequence
            [ codegen expr
            , emitBetween' "[" "]" $ codegen inner
            ]
        Literal _ value' -> codegen value'
        Parenthesis _ expr -> emitBetween' "(" ")" $ codegen expr
        Structure _ structure' -> codegen structure'
        UnaryOp _ symbol' expr -> mconcat <$> sequence
            [ emitM $ unaryToText symbol'
            , codegen expr
            ]
        Variable _ name' -> emitM name'

-- TODO: Add let ... in ... so that we can declare variables and functions?
data Literal
    = Char Char
    | Double Double
    | Integer Integer
    | Text Text
    deriving (Eq, Show)

instance Codegen Literal where
    type GeneratorState Literal = ()

    codegen = \case
        Char c -> emitBetween' "'" "'" $ emitM (Text.singleton c)
        Double d -> emitM $ show d
        Integer i -> emitM $ show i
        Text t -> emitBetween' "\""  "\"" $ emitM t

instance FromJSON Literal where
    parseJSON = withObject "Language.LowCode.Logic.AST.Literal" \o -> o .: "type" >>= \case
        "char"   -> Char       <$> o .: "value"
        "double" -> Double     <$> o .: "value"
        "integer"-> Integer    <$> o .: "value"
        "text"   -> Text       <$> o .: "value"
        other    -> fail $
            "Expected 'char', 'double','integer' or 'text', but got '" <> other <> "'."

instance ToJSON Literal where
    toJSON = \case
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
        Text t -> object
            [ "type"   .= String "text"
            , "value"  .= t
            ]

data Structure metadata
    = Algebraic metadata Name [Expression metadata]
    | Array metadata [Expression metadata]
    | Record metadata Name [(Name, Expression metadata)]
    deriving (Eq, Functor, Show)

unit :: Structure Type
unit = Algebraic unitType "Unit" []

instance HasMetadata Structure where
    getMetadata = \case
        Algebraic m _ _ -> m
        Array m _ -> m
        Record m _ _ -> m

instance Codegen (Structure metadata) where
    type GeneratorState (Structure metadata) = ()

    codegen = \case
        Algebraic _ name fields -> codegenAlgebraic name fields
        Array _ a -> emitBetween' "[" "]" $ a `separatedBy'` ", "
        Record _ r fs -> codegenRecord r fs
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

instance FromJSON (Structure ()) where
    parseJSON = withObject "Language.LowCode.Logic.AST.Structure" \o -> o .: "type" >>= \case
        "adt"    -> Algebraic () <$> o .: "name"  <*> o .: "fields"
        "record" -> Record    () <$> o .: "name"  <*> o .: "fields"
        "array"  -> Array     () <$> o .: "value"
        other    -> fail $
            "Expected 'adt', 'array' or 'record', but got '" <> other <> "'."

instance ToJSON (Structure metadata) where
    toJSON = \case
        Algebraic _ name constructors -> object
            [ "type"   .= String "adt"
            , "name"   .= name
            , "fields" .= constructors
            ]
        Array _ a -> object
            [ "type"   .= String "array"
            , "value"  .= a
            ]
        Record _ recordName fields -> object
            [ "type"   .= String "record"
            , "name"   .= recordName
            , "fields" .= fields
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

parseExpression :: Text -> Either Text (Expression ())
parseExpression = first (toText . errorBundlePretty) . parse (expression0 <* eof) ""

-- Reference:
-- https://en.wikipedia.org/wiki/Operator-precedence_parser#Precedence_climbing_method
expression0 :: Parser (Expression ())
expression0 = expression1 0 =<< primary

expression1 :: Int -> Expression () -> Parser (Expression ())
expression1 !minPrecedence lhs = maybe (pure lhs) (loop lhs) =<< peek
  where
    loop lhs' op
        | precedence op >= minPrecedence = do
            space
            symbol <- binaryOperator
            rhs <- primary
            lookaheadMaybe <- peek
            case lookaheadMaybe of
                Nothing -> pure $ BinaryOp () lhs' symbol rhs
                Just lookahead -> do
                    (rhs', lookaheadMaybe') <- loop' rhs op lookahead
                    let result = BinaryOp () lhs' symbol rhs'
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

primary :: Parser (Expression ())
primary = space *> (secondary =<< choice [value, unary, parenthesis]) <* space

secondary :: Expression () -> Parser (Expression ())
secondary lhs = do
    space
    rhs <- optional (secondary =<< choice [access' lhs, call' lhs, index' lhs])
    pure $ fromMaybe lhs rhs
  where
    access' lhs' = Access () lhs' <$> access
    call' lhs' = Call () lhs' <$> tuple
    index' lhs' = Index () lhs' <$> index

access :: Parser Name
access = char '.' *> space *> variableName

bracket :: Char -> Char -> Parser a -> Parser a
bracket left right = between (char left) (space *> char right)

tuple :: Parser [Expression ()]
tuple = bracket '(' ')' (expression0 `sepBy` char ',')

index :: Parser (Expression ())
index = bracket '[' ']' expression0

array :: Parser [Expression ()]
array = bracket '[' ']' (expression0 `sepEndBy` char ',')

unary :: Parser (Expression ())
unary = liftA2 (UnaryOp ()) unaryOperator primary

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

value :: Parser (Expression ())
value = structure <|> constant <|> variable

structure :: Parser (Expression ())
structure = Structure () <$> choice
    [ Array   () <$> array
    , record'    <$> try record
    ]
  where
    record' = uncurry (Record ())

constant :: Parser (Expression ())
constant = Literal () <$> choice
    [ Char    <$> char''
    , Double  <$> try Lexer.float
    , Integer <$> try integer
    , Text    <$> text
    ]

variable :: Parser (Expression ())
variable = Variable () <$> variableName

parenthesis :: Parser (Expression ())
parenthesis = bracket '(' ')' (Parenthesis () <$> expression0)

char'' :: Parser Char
char'' = between (char '\'') (char '\'') Lexer.charLiteral

text :: Parser Text
text = toText <$> (char '"' *> manyTill Lexer.charLiteral (char '"'))

variableName :: Parser Text
variableName = do
    head' <- letterChar <|> char '_'
    tail' <- takeWhileP Nothing (\c -> Char.isAlphaNum c || c == '_')
    pure $ Text.cons head' tail'

record :: Parser (Name, [(Name, Expression ())])
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
