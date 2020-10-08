module Language.LowCode.Logic.AST
    ( module Language.Common
    , HasMetadata (..)
    , Field (..)
    , Constructor (..)
    , Function (..)
    , MatchPattern (..)
    , patternToExpr
    , AST (..)
    , Expression (..)
    , Literal (..)
    , Structure (..)
    , parseFunction
    , function
    , parseAst
    , parseExpression
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
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Text.Megaparsec hiding (match, unexpected)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

import           Language.Codegen
import           Language.Common
import           Language.Emit
import           Language.LowCode.Logic.Parser
import           Language.LowCode.Logic.Type
import           Utility (biject, withTag)

class HasMetadata f where
    getMetadata  :: f a -> a

data Function exprMetadata metadata
    = Function !metadata !Name !Type ![Name] !(AST exprMetadata metadata)
    deriving (Eq, Functor, Show)

instance (FromJSON metadata) => FromJSON (Function () metadata) where
    parseJSON = withObject "Language.LowCode.Logic.AST.Function" \o ->
        Function <$> o .: "metadata"
                 <*> o .: "name"
                 <*> o .: "type"
                 <*> o .: "arguments"
                 <*> o .: "body"

instance (ToJSON expressionMetadata, ToJSON metadata) => ToJSON (Function expressionMetadata metadata) where
    toJSON (Function metadata name returnType arguments body) = object
        [ "metadata"  .= metadata
        , "name"      .= String name
        , "type"      .= returnType
        , "arguments" .= arguments
        , "body"      .= body
        ]

data MatchPattern
    = AlgebraicPattern !(Constructor MatchPattern)
    | ArrayPattern ![MatchPattern]
    | LiteralPattern !Literal
    | NamePattern !Name
    | RecordPattern ![Field MatchPattern]
    deriving (Eq, Show)

instance FromJSON MatchPattern where
    parseJSON = withObject "Language.LowCode.Logic.AST.MatchPattern" \o -> o .: "tag" >>= \case
        "adt"     -> AlgebraicPattern <$> o .: "constructor"
        "array"   -> ArrayPattern     <$> o .: "positions"
        "literal" -> LiteralPattern   <$> o .: "value"
        "name"    -> NamePattern      <$> o .: "name"
        "record"  -> RecordPattern    <$> o .: "fields"
        other     -> fail $
            "Expected 'adt', 'array', 'default', 'literal' or 'record', but got\
            \ '" <> other <> "'."

instance ToJSON MatchPattern where
    toJSON = \case
        AlgebraicPattern constructor -> withTag "adt" ["constructor" .= constructor]
        ArrayPattern positions -> withTag "array" ["positions" .= positions]
        LiteralPattern value' -> withTag "literal" ["value" .= value']
        NamePattern name -> withTag "name" ["name" .= name]
        RecordPattern fields -> withTag "record" ["fields" .= fields]

patternToExpr :: MatchPattern -> Expression ()
patternToExpr = \case
    AlgebraicPattern constructor -> Structure () $ Algebraic () (patternToExpr <$> constructor)
    ArrayPattern positions -> Structure () $ Array () (patternToExpr <$> positions)
    LiteralPattern lit -> Literal () lit
    NamePattern name -> Variable () name
    RecordPattern fields -> Structure () $ Record () (patternToExpr <<$>> fields)

data AST exprMetadata metadata
    -- | Assigns a new value to the first expression, and what follows after.
    = Assign !metadata !(Expression exprMetadata) !(Expression exprMetadata) !(AST exprMetadata metadata)
    -- | Represents the end of a cycle.
    | End !metadata
    -- | Executes a raw expression. Useful for Call.
    | Expression !metadata !(Expression exprMetadata) !(AST exprMetadata metadata)
    -- | Allows a condition to be tested, and contains the true branch, false
    -- branch and what follows after the branches.
    | If !metadata !(Expression exprMetadata) !(AST exprMetadata metadata) !(AST exprMetadata metadata) !(AST exprMetadata metadata)
    -- | Represents a match pattern, similar to a "case of" or a "switch" structure.
    | Match !metadata !(Expression exprMetadata) ![(MatchPattern, AST exprMetadata metadata)] !(AST exprMetadata metadata)
    -- | Exits the function, optionally returning a value.
    | Return !metadata !(Maybe (Expression exprMetadata))
    -- | Declares a variable with the specified name and type, followed by the
    -- remainder of the cycle.
    | Var !metadata !Name !Type !(Expression exprMetadata) !(AST exprMetadata metadata)
    -- | Represents a condition which should be run while a predicate is not
    -- met, followed by the remainder of the cycle.
    | While !metadata !(Expression exprMetadata) !(AST exprMetadata metadata) !(AST exprMetadata metadata)
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
        Assign metadata left right next -> withTag "assign"
            [ "metadata"        .= metadata
            , "leftExpression"  .= left
            , "rightExpression" .= right
            , "nextAst"         .= next
            ]
        End metadata -> withTag "end"
            [ "metadata"        .= metadata
            ]
        Expression metadata expr next -> withTag "expression"
            [ "metadata"        .= metadata
            , "expression"      .= expr
            , "nextAst"         .= next
            ]
        If metadata expr true false next -> withTag "if"
            [ "metadata"        .= metadata
            , "expression"      .= expr
            , "trueBranchAst"   .= true
            , "falseBranchAst"  .= false
            , "nextAst"         .= next
            ]
        Match metadata expr branches next -> withTag "match"
            [ "metadata"        .= metadata
            , "expression"      .= expr
            , "branches"        .= branches
            , "nextAst"         .= next
            ]
        Return metadata expr -> withTag "return"
            [ "metadata"        .= metadata
            , "expression"      .= expr
            ]
        Var metadata name type' expr next -> withTag "var"
            [ "metadata"        .= metadata
            , "name"            .= name
            , "type"            .= type'
            , "expression"      .= expr
            , "nextAst"         .= next
            ]
        While metadata expr body next -> withTag "while"
            [ "metadata"        .= metadata
            , "expression"      .= expr
            , "whileAst"        .= body
            , "nextAst"         .= next
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
    = Access !metadata !(Expression metadata) !Name
    | BinaryOp !metadata !(Expression metadata) !BinarySymbol !(Expression metadata)
    | Call !metadata !(Expression metadata) ![(Expression metadata)]
    | Index !metadata !(Expression metadata) !(Expression metadata)
    | Literal !metadata !Literal
    | Parenthesis !metadata !(Expression metadata)
    | Structure !metadata !(Structure metadata)
    | UnaryOp !metadata !UnarySymbol !(Expression metadata)
    | Variable !metadata !Name
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
        Access _ expr name -> withTag "access"
            [ "expression"      .= expr
            , "name"            .= name
            ]
        BinaryOp _ left symbol' right -> withTag "binaryOp"
            [ "leftExpression"  .= left
            , "symbol"          .= symbol'
            , "rightExpression" .= right
            ]
        Call _ expr arguments -> withTag "call"
            [ "expression"      .= expr
            , "arguments"       .= arguments
            ]
        Index _ left right -> withTag "index"
            [ "leftExpression"  .= left
            , "rightExpression" .= right
            ]
        Literal _ value' -> withTag "literal"
            [ "value"           .= value'
            ]
        Parenthesis _ expr -> withTag "parenthesis"
            [ "expression"      .= expr
            ]
        Structure _ structure' -> withTag "structure"
            [ "structure"       .= structure'
            ]
        UnaryOp _ symbol' expr -> withTag "unaryOp"
            [ "symbol"          .= symbol'
            , "expression"      .= expr
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

data Literal
    = Char !Char
    | Double !Double
    | Integer !Integer
    | Text !Text
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
        "char"   -> Char    <$> o .: "value"
        "double" -> Double  <$> o .: "value"
        "integer"-> Integer <$> o .: "value"
        "text"   -> Text    <$> o .: "value"
        other    -> fail $
            "Expected 'char', 'double','integer' or 'text', but got '" <> other <> "'."

instance ToJSON Literal where
    toJSON = \case
        Char c -> withTag "char" ["value" .= c]
        Double d -> withTag "double" ["value" .= d]
        Integer i -> withTag "integer" ["value" .= i]
        Text t -> withTag "text" ["value" .= t]

data Structure metadata
    = Algebraic !metadata !(Constructor (Expression metadata))
    | Array !metadata ![Expression metadata]
    | Record !metadata ![Field (Expression metadata)]
    deriving (Eq, Functor, Show)

instance HasMetadata Structure where
    getMetadata = \case
        Algebraic m _ -> m
        Array m _ -> m
        Record m _ -> m

instance Codegen (Structure metadata) where
    type GeneratorState (Structure metadata) = ()

    codegen = \case
        Algebraic _ constructor -> codegenAlgebraic constructor
        Array _ a -> emitBetween' "[" "]" $ a `separatedBy'` ", "
        Record _ fs -> codegenRecord fs
      where
        codegenAlgebraic (Constructor adtName name value') = mconcat <$> sequence
            [ emitM adtName
            , emitM "::"
            , emitM name
            , maybe (emitM "") (emitBetween' "(" ")" . codegen) value'
            ]

        codegenField (Field fieldName expr) = mconcat <$> sequence
            [ emitM fieldName
            , emitM " = "
            , codegen expr
            ]

        codegenRecord fields =
            emitBetween' "{" "}" $ separatedByF codegenField (emit ", ") fields

instance FromJSON (Structure ()) where
    parseJSON = withObject "Language.LowCode.Logic.AST.Structure" \o -> o .: "tag" >>= \case
        "adt"    -> Algebraic () <$> o .: "constructor"
        "array"  -> Array     () <$> o .: "positions"
        "record" -> Record    () <$> o .: "fields"
        other    -> fail $
            "Expected 'adt', 'array' or 'record', but got '" <> other <> "'."

instance ToJSON (Structure metadata) where
    toJSON = \case
        Algebraic _ constructor -> withTag "adt" ["constructor" .= constructor]
        Array _ a -> withTag "array" ["positions" .= a]
        Record _ fields -> withTag "record" ["fields" .= fields]

data Associativity = LeftAssoc | RightAssoc deriving (Eq, Show)

type Precedence = Int

data Operator = Operator
    { associativity :: !Associativity
    , precedence    :: !Precedence
    } deriving (Eq, Show)

parseFunction :: Text -> Either Text (Function () ())
parseFunction = parse' function

function :: Parser (Function () ())
function = label "function declaration" (try function' <|> functionSugar)

function' :: Parser (Function () ())
function' = do
    type' <- typeName
    name <- variableName
    args <- tuple variableName
    body <- brackets ast
    pure $ Function () name type' args body

functionSugar :: Parser (Function () ())
functionSugar = do
    name <- variableName
    (types, args) <- unzip <$> tuple (liftA2 (,) typeName variableName)
    symbol "->"
    ret <- typeName
    body <- brackets ast
    pure $ Function () name (FunctionType types ret) args body

parseAst :: Text -> Either Text (AST () ())
parseAst = parse' ast

ast :: Parser (AST () ())
ast = option (End ()) $ choice [assign, try expression, if', match, return', while, var]

assign :: Parser (AST () ())
assign = do
    left <- try $ leftOfAssignment <* void (lexeme (char '='))
    right <- expression0
    endl
    next <- ast
    pure $ Assign () left right next
  where
    leftOfAssignment = secondary =<< variable

expression :: Parser (AST () ())
expression = liftA2 (Expression ()) (expression0 <* endl) ast

if' :: Parser (AST () ())
if' = do
    symbol "if"
    cond <- expression0
    trueBranch <- brackets ast
    falseBranch <- option (End ()) (symbol "else" *> (brackets ast <|> if'))
    next <- ast
    pure $ If () cond trueBranch falseBranch next

match :: Parser (AST () ())
match = do
    symbol "match"
    cond <- expression0
    pats <- brackets $ flip sepEndBy (lexeme (char ',')) do
        pat <- pattern
        branch <- brackets ast
        pure (pat, branch)
    next <- ast
    pure $ Match () cond pats next

return' :: Parser (AST () ())
return' = Return () <$> (symbol "return" *> optional expression0 <* endl)

var :: Parser (AST () ())
var = do
    type' <- typeName
    name <- variableName
    void (lexeme (char '='))
    value' <- expression0
    endl
    next <- ast
    pure $ Var () name type' value' next

while :: Parser (AST () ())
while = do
    symbol "while"
    cond <- expression0
    loop <- brackets ast
    next <- ast
    pure $ While () cond loop next

pattern :: Parser MatchPattern
pattern = choice [algebraicP, arrayP, literalP, nameP, recordP]
  where
    algebraicP = AlgebraicPattern <$> algebraic pattern
    arrayP = ArrayPattern <$> array pattern
    literalP = LiteralPattern <$> literal
    nameP = NamePattern <$> variableName
    recordP = RecordPattern <$> record pattern

literal :: Parser Literal
literal = choice
    [ Char    <$> char''
    , Double  <$> try Lexer.float
    , Integer <$> try integer
    , Text    <$> text
    ]

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

parseExpression :: Text -> Either Text (Expression ())
parseExpression = parse' expression0

-- Reference:
-- https://en.wikipedia.org/wiki/Operator-precedence_parser#Precedence_climbing_method
expression0 :: Parser (Expression ())
expression0 = expression1 0 =<< primary

expression1 :: Int -> Expression () -> Parser (Expression ())
expression1 !minPrecedence lhs = maybe (pure lhs) (loop lhs) =<< peek
  where
    loop lhs' op
        | precedence op >= minPrecedence = do
            symbol' <- binaryOperator
            rhs <- primary
            lookaheadMaybe <- peek
            case lookaheadMaybe of
                Nothing -> pure $ BinaryOp () lhs' symbol' rhs
                Just lookahead -> do
                    (rhs', lookaheadMaybe') <- loop' rhs op lookahead
                    let result = BinaryOp () lhs' symbol' rhs'
                    maybe (pure result) (loop result) lookaheadMaybe'
        | otherwise = pure lhs'

    loop' rhs op lookahead
        | (precedence lookahead > precedence op)
            || (precedence lookahead == precedence op && associativity lookahead == RightAssoc) = do
            rhs' <- expression1 (precedence lookahead) rhs
            lookaheadMaybe <- peek
            maybe (pure (rhs', Nothing)) (loop' rhs' op) lookaheadMaybe
        | otherwise = pure (rhs, Just lookahead)

    peek = optional $ lookAhead $ binarySymbolToOperator <$> binaryOperator

primary :: Parser (Expression ())
primary = lexeme (secondary =<< choice [value, unary, Parenthesis () <$> parenthesis expression0])

secondary :: Expression () -> Parser (Expression ())
secondary lhs = lexeme do
    rhs <- optional (secondary =<< choice [access' lhs, call' lhs, index' lhs])
    pure $ fromMaybe lhs rhs
  where
    access' lhs' = Access () lhs' <$> access
    call' lhs' = Call () lhs' <$> tuple expression0
    index' lhs' = Index () lhs' <$> index

access :: Parser Name
access = lexeme (char '.') *> variableName

index :: Parser (Expression ())
index = braces expression0

unary :: Parser (Expression ())
unary = liftA2 (UnaryOp ()) unaryOperator primary

integer :: Parser Integer
integer = lexeme do
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
value = structure <|> Literal () <$> literal <|> variable

structure :: Parser (Expression ())
structure = Structure () <$> choice
    [ Algebraic () <$> algebraic expression0
    , Array     () <$> array expression0
    , Record    () <$> record expression0
    ]

variable :: Parser (Expression ())
variable = Variable () <$> variableName

char'' :: Parser Char
char'' = between (char '\'') (lexeme (char '\'')) Lexer.charLiteral

text :: Parser Text
text = toText <$> (char '"' *> manyTill Lexer.charLiteral (char '"'))

errorUnaryNotFound :: (IsString s, Semigroup s) => s -> s
errorUnaryNotFound symbol' = "Undefined unary operator '" <> symbol' <> "'."

errorBinaryNotFound :: (IsString s, Semigroup s) => s -> s
errorBinaryNotFound symbol' = "Undefined binary operator '" <> symbol' <> "'."

isOperatorSymbol :: Char -> Bool
isOperatorSymbol c = Set.member c allowed
  where
    allowed :: Set Char
    allowed = Set.fromList $ toString $ Text.concat (Map.keys mapTextToUnary <> Map.keys mapTextToBinary)

unaryOperator :: Parser UnarySymbol
unaryOperator = do
    symbol' <- lexeme (takeWhile1P (Just "unary symbol") isOperatorSymbol)
    maybe (fail $ errorUnaryNotFound $ toString symbol') pure $ Map.lookup symbol' mapTextToUnary

binaryOperator :: Parser BinarySymbol
binaryOperator = do
    symbol' <- lexeme (takeWhile1P (Just "binary symbol") isOperatorSymbol)
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
