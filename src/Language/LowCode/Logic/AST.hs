module Language.LowCode.Logic.AST
    ( module Language.Common
    , HasMetadata (..)
    , Field (..)
    , Constructor (..)
    , Function (..)
    , MatchPattern (..)
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
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import           Text.Megaparsec hiding (unexpected)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

import           Language.Codegen
import           Language.Common
import           Language.Emit
import           Utility (biject, withTag)

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

data Function exprMetadata metadata
    = Function !metadata !Name !Type ![Name] !(AST exprMetadata metadata)
    deriving (Eq, Functor, Show)

instance (FromJSON metadata) => FromJSON (Function () metadata) where
    parseJSON = withObject "Language.LowCode.Logic.AST.Function" \o ->
        Function <$> o .: "metadata"
                 <*> o .: "name"
                 <*> o .: "returnType"
                 <*> o .: "arguments"
                 <*> o .: "nextAst"

instance (ToJSON expressionMetadata, ToJSON metadata) => ToJSON (Function expressionMetadata metadata) where
    toJSON (Function metadata name returnType arguments ast) = object
        [ "metadata"   .= metadata
        , "name"       .= String name
        , "returnType" .= returnType
        , "arguments"  .= arguments
        , "nextAst"    .= ast
        ]

data MatchPattern
    = AlgebraicPattern Name [MatchPattern]
    | ArrayPattern [MatchPattern]
    | LiteralPattern Literal
    | NamePattern Name
    | RecordPattern Name [MatchPattern]
    deriving (Eq, Show)

instance FromJSON MatchPattern where
    parseJSON = withObject "Language.LowCode.Logic.AST.MatchExpr" \o -> o .: "tag" >>= \case
        "adt"     -> AlgebraicPattern <$> o .: "constructorName" <*> o .: "fields"
        "array"   -> ArrayPattern <$> o .: "positions"
        "literal" -> LiteralPattern <$> o .: "value"
        "name"    -> NamePattern <$> o .: "name"
        "record"  -> RecordPattern <$> o .: "recordName" <*> o .: "fields"
        other     -> fail $
            "Expected 'adt', 'array', 'default', 'literal' or 'record', but got\
            \ '" <> other <> "'."

instance ToJSON MatchPattern where
    toJSON = \case
        AlgebraicPattern name fields -> withTag "adt"
            [ "constructorName" .= name
            , "fields"          .= fields
            ]
        ArrayPattern positions -> withTag "array"
            [ "positions"       .= positions
            ]
        LiteralPattern value' -> withTag "literal"
            [ "value"           .= value'
            ]
        NamePattern name -> withTag "name"
            [ "name"            .= name
            ]
        RecordPattern name fields -> withTag "record"
            [ "recordName"      .= name
            , "fields"          .= fields
            ]

data AST exprMetadata metadata
    -- | Assigns a new value to the first expression, and what follows after.
    = Assign metadata (Expression exprMetadata) (Expression exprMetadata) (AST exprMetadata metadata)
    -- | Represents the end of a cycle.
    | End metadata
    -- | Executes a raw expression. Useful for Call.
    | Expression metadata (Expression exprMetadata) (AST exprMetadata metadata)
    -- | Allows a condition to be tested, and contains the true branch, false
    -- branch and what follows after the branches.
    | If metadata (Expression exprMetadata) (AST exprMetadata metadata) (AST exprMetadata metadata) (AST exprMetadata metadata)
    -- | Represents a match pattern, similar to a "case of" or a "switch" structure.
    | Match metadata (Expression exprMetadata) [(MatchPattern, AST exprMetadata metadata)] (AST exprMetadata metadata)
    -- | Exits the function, optionally returning a value.
    | Return metadata (Maybe (Expression exprMetadata))
    -- | Declares a variable with the specified name and type, followed by the
    -- remainder of the cycle.
    | Var metadata Name !Type (Expression exprMetadata) (AST exprMetadata metadata)
    -- | Represents a condition which should be run while a predicate is not
    -- met, followed by the remainder of the cycle.
    | While metadata (Expression exprMetadata) (AST exprMetadata metadata) (AST exprMetadata metadata)
    deriving (Eq, Functor, Show)

instance HasMetadata (AST expressionMetadata) where
    getMetadata = \case
        Assign m _ _ _ -> m
        End m -> m
        Expression m _ _ -> m
        If m _ _ _ _ -> m
        Match m _ _ _ -> m
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
        "match"       -> Match      <$> o .: "metadata"
                                    <*> o .: "expression"
                                    <*> o .: "branches"
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
            "Unknown tag '" <> other <> "'. Available tags are: 'assign', 'end',\
            \ 'expression', 'if', 'match', 'return', 'start', 'var' and 'while'."

instance (ToJSON expressionMetadata, ToJSON metadata) => ToJSON (AST expressionMetadata metadata) where
    toJSON = \case
        Assign metadata left right ast -> withTag "assign"
            [ "metadata"        .= metadata
            , "leftExpression"  .= left
            , "rightExpression" .= right
            , "nextAst"         .= ast
            ]
        End metadata -> withTag "end"
            [ "metadata"        .= metadata
            ]
        Expression metadata expression ast -> withTag "expression"
            [ "metadata"        .= metadata
            , "expression"      .= expression
            , "nextAst"         .= ast
            ]
        If metadata expression true false ast -> withTag "if"
            [ "metadata"        .= metadata
            , "expression"      .= expression
            , "trueBranchAst"   .= true
            , "falseBranchAst"  .= false
            , "nextAst"         .= ast
            ]
        Match metadata expr branches ast -> withTag "match"
            [ "metadata"        .= metadata
            , "expression"      .= expr
            , "branches"        .= branches
            , "nextAst"         .= ast
            ]
        Return metadata expression -> withTag "return"
            [ "metadata"        .= metadata
            , "expression"      .= expression
            ]
        Var metadata name type' expression ast -> withTag "var"
            [ "metadata"        .= metadata
            , "name"            .= name
            , "type"            .= type'
            , "expression"      .= expression
            , "nextAst"         .= ast
            ]
        While metadata expression body ast -> withTag "while"
            [ "metadata"        .= metadata
            , "expression"      .= expression
            , "whileAst"        .= body
            , "nextAst"         .= ast
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
        Access _ expression name -> withTag "access"
            [ "expression"      .= expression
            , "name"            .= name
            ]
        BinaryOp _ left symbol' right -> withTag "binaryOp"
            [ "leftExpression"  .= left
            , "symbol"          .= symbol'
            , "rightExpression" .= right
            ]
        Call _ expression arguments -> withTag "call"
            [ "expression"      .= expression
            , "arguments"       .= arguments
            ]
        Index _ left right -> withTag "index"
            [ "leftExpression"  .= left
            , "rightExpression" .= right
            ]
        Literal _ value' -> withTag "literal"
            [ "value"           .= value'
            ]
        Parenthesis _ expression -> withTag "parenthesis"
            [ "expression"      .= expression
            ]
        Structure _ structure' -> withTag "structure"
            [ "structure"       .= structure'
            ]
        UnaryOp _ symbol' expression -> withTag "unaryOp"
            [ "symbol"          .= symbol'
            , "expression"      .= expression
            ]
        Variable _ name -> withTag "variable"
            [ "name"            .= name
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
    parseJSON = withObject "Language.LowCode.Logic.AST.Literal" \o -> o .: "tag" >>= \case
        "char"   -> Char       <$> o .: "value"
        "double" -> Double     <$> o .: "value"
        "integer"-> Integer    <$> o .: "value"
        "text"   -> Text       <$> o .: "value"
        other    -> fail $
            "Expected 'char', 'double','integer' or 'text', but got '" <> other <> "'."

instance ToJSON Literal where
    toJSON = \case
        Char c -> withTag "char" ["value" .= c]
        Double d -> withTag "double" ["value" .= d]
        Integer i -> withTag "integer" ["value" .= i]
        Text t -> withTag "text" ["value" .= t]

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
    parseJSON = withObject "Language.LowCode.Logic.AST.Structure" \o -> o .: "tag" >>= \case
        "adt"    -> Algebraic () <$> o .: "name"  <*> o .: "fields"
        "record" -> Record    () <$> o .: "name"  <*> o .: "fields"
        "array"  -> Array     () <$> o .: "value"
        other    -> fail $
            "Expected 'adt', 'array' or 'record', but got '" <> other <> "'."

instance ToJSON (Structure metadata) where
    toJSON = \case
        Algebraic _ name constructors -> withTag "adt"
            [ "name"   .= name
            , "fields" .= constructors
            ]
        Array _ a -> withTag "array"
            [ "value"  .= a
            ]
        Record _ recordName fields -> withTag "record"
            [ "name"   .= recordName
            , "fields" .= fields
            ]

-- FIXME: Suppose an AlgebraicType Foo with a constructor Bar Foo. How do we
-- encode it? Not the way it's actually implemented, because it will stack
-- overflow trying to find it's own type infinitely! The best way to fix it
-- would perhaps be to simply refer to its own (module-qualified) name.
--   RecordType likely will have the same problem if it tries to refer to itself
-- and so will probably require the same fix.
--   Note that the module will still contain a Map Name [Constructor] that can
--   be used to lookup for the constructors of the ADT.
-- As for the qualified name, I suggest creating a new record called Import,
-- parametrized over a type variable a, which will contain the module of type a,
-- and an ADT Qualification, with constructors Qualified Text and Unqualified.
--   Notice this will also solve another problem, which is checking for types
-- with the same name but different fields, which shouldn't be allowed.
-- TODO: Should RecordType keep track of its name? Or should we perhaps add a
-- new type (such as struct or class) for this?
-- TODO: Should AlgebraicType constructors be named (like F# or Rust does)? This
-- will allow for better JS name generation. Alternatively, we could also allow
-- just one Type per Constructor (again, like F# or Rust), and make use of
-- Records in case that the user needs more than one field in the constructor.
-- TODO: Should we make a tuple type? Or should it be defined as a simple ADT,
-- like PureScript does?
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
    parseJSON = withObject "Language.LowCode.Logic.AST.VariableType" \o -> o .: "tag" >>= \case
        "adt"      -> AlgebraicType <$> o .: "name" <*> o .: "constructors"
        "array"    -> ArrayType <$> o .: "elements"
        "char"     -> pure CharType
        "double"   -> pure DoubleType
        "function" -> FunctionType <$> o .: "arguments" <*> o .: "return"
        "integer"  -> pure IntegerType
        "record"   -> RecordType <$> o .: "fields"
        "text"     -> pure TextType
        other      -> fail $
            "Expected 'array', 'char', 'double', 'function','integer', 'record'\
            \ or 'text', but got '" <> other <> "'."

instance ToJSON Type where
    toJSON = \case
        AlgebraicType name constructors -> withTag "adt"
            [ "name"         .= name
            , "constructors" .= constructors
            ]
        ArrayType elements -> withTag "array"
            [ "elements"     .= elements
            ]
        CharType    -> withTag "char" []
        DoubleType  -> withTag "double" []
        FunctionType arguments return' -> withTag "function"
            [ "arguments"     .= arguments
            , "return"        .= return'
            ]
        IntegerType -> withTag "integer" []
        RecordType fieldsType -> withTag "record"
            [ "fields"      .= fieldsType
            ]
        TextType    -> withTag "text" []

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
    radixMaybe <- optional $ try (mkRadix <* char' 'r')
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
