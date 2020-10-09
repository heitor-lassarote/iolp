module Language.LowCode.Logic.AST
    ( module Language.Common
    , HasMetadata (..)
    , Field (..)
    , Constructor (..)
    , Function (..)
    , MatchPattern (..)
    , Branch (..)
    , patternToExpr
    , AST (..)
    , Expression (..)
    , Literal (..)
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
import           Language.LowCode.Logic.Structure
import           Language.LowCode.Logic.Type
import           Utility (biject, withTag)

class HasMetadata f where
    getMetadata  :: f a -> a

data Function exprMetadata
    = Function !Name !Type ![Name] ![AST exprMetadata]
    deriving (Eq, Functor, Show)

instance FromJSON (Function ()) where
    parseJSON = withObject "Language.LowCode.Logic.AST.Function" \o ->
        Function <$> o .: "name"
                 <*> o .: "type"
                 <*> o .: "arguments"
                 <*> o .: "body"

instance ToJSON (Function expressionMetadata) where
    toJSON (Function name returnType arguments body) = object
        [ "name"      .= String name
        , "type"      .= returnType
        , "arguments" .= arguments
        , "body"      .= body
        ]

data MatchPattern
    = LiteralPattern !Literal
    | NamePattern !Name
    | StructurePattern !(Structure MatchPattern)
    deriving (Eq, Show)

instance FromJSON MatchPattern where
    parseJSON = withObject "Language.LowCode.Logic.AST.MatchPattern" \o -> o .: "tag" >>= \case
        "literal"   -> LiteralPattern   <$> o .: "value"
        "name"      -> NamePattern      <$> o .: "name"
        "structure" -> StructurePattern <$> o .: "struct"
        other     -> fail $
            "Expected 'literal', 'name' or 'structure', but got '" <> other <> "'."

instance ToJSON MatchPattern where
    toJSON = \case
        LiteralPattern value' -> withTag "literal" ["value" .= value']
        NamePattern name -> withTag "name" ["name" .= name]
        StructurePattern struct -> withTag "structure" ["struct" .= struct]

patternToExpr :: MatchPattern -> Expression ()
patternToExpr = \case
    LiteralPattern lit -> Literal () lit
    NamePattern name -> Variable () name
    StructurePattern struct -> Structure () (patternToExpr <$> struct)

data Branch exprMetadata
    = Branch MatchPattern [AST exprMetadata]
    deriving (Eq, Functor, Show)

instance FromJSON (Branch ()) where
    parseJSON = withObject "Language.LowCode.Logic.AST.Branch" \o ->
        Branch <$> o .: "pattern"
               <*> o .: "ast"

instance ToJSON (Branch exprMetadata) where
    toJSON (Branch p a) = object
        [ "pattern" .= p
        , "ast"     .= a
        ]

data AST exprMetadata
    -- | Assigns a new value to the first expression.
    = Assign !(Expression exprMetadata) !(Expression exprMetadata)
    -- | Executes a raw expression. Useful for Call.
    | Expression !(Expression exprMetadata)
    -- | Allows a condition to be tested, and contains the true branch and false branch.
    | If !(Expression exprMetadata) ![AST exprMetadata] ![AST exprMetadata]
    -- | Represents a match pattern, similar to a "case of" or a "switch" structure.
    | Match !(Expression exprMetadata) ![Branch exprMetadata]
    -- | Exits the function, optionally returning a value.
    | Return !(Maybe (Expression exprMetadata))
    -- | Declares a variable with the specified name and type.
    | Var !Name !Type !(Expression exprMetadata)
    -- | Represents a condition which should be run while a predicate is not met.
    | While !(Expression exprMetadata) ![AST exprMetadata]
    deriving (Eq, Functor, Show)

instance FromJSON (AST ()) where
    parseJSON = withObject "Language.LowCode.Logic.AST.AST" \o -> o .: "tag" >>= \case
        "assign"      -> Assign     <$> o .: "leftExpression"
                                    <*> o .: "rightExpression"
        "expression"  -> Expression <$> o .: "expression"
        "if"          -> If         <$> o .: "expression"
                                    <*> o .: "trueBranchAst"
                                    <*> o .: "falseBranchAst"
        "match"       -> Match      <$> o .: "expression"
                                    <*> o .: "branches"
        "return"      -> Return     <$> o .: "expression"
        "var"         -> Var        <$> o .: "name"
                                    <*> o .: "type"
                                    <*> o .: "expression"
        "while"       -> While      <$> o .: "expression"
                                    <*> o .: "whileAst"
        other         -> fail $
            "Unknown tag '" <> other <> "'. Available tags are: 'assign',\
            \ 'expression', 'if', 'match', 'return', 'start', 'var' and 'while'."

instance ToJSON (AST expressionMetadata) where
    toJSON = \case
        Assign left right -> withTag "assign"
            [ "leftExpression"  .= left
            , "rightExpression" .= right
            ]
        Expression expr -> withTag "expression"
            [ "expression"      .= expr
            ]
        If expr true false -> withTag "if"
            [ "expression"      .= expr
            , "trueBranchAst"   .= true
            , "falseBranchAst"  .= false
            ]
        Match expr branches -> withTag "match"
            [ "expression"      .= expr
            , "branches"        .= branches
            ]
        Return expr -> withTag "return"
            [ "expression"      .= expr
            ]
        Var name type' expr -> withTag "var"
            [ "name"            .= name
            , "type"            .= type'
            , "expression"      .= expr
            ]
        While expr body -> withTag "while"
            [ "expression"      .= expr
            , "whileAst"        .= body
            ]

data Expression metadata
    = Access !metadata !(Expression metadata) !Name
    | BinaryOp !metadata !(Expression metadata) !BinarySymbol !(Expression metadata)
    | Call !metadata !(Expression metadata) ![(Expression metadata)]
    | Index !metadata !(Expression metadata) !(Expression metadata)
    | Literal !metadata !Literal
    | Parenthesis !metadata !(Expression metadata)
    | Structure !metadata !(Structure (Expression metadata))
    | UnaryOp !metadata !UnarySymbol !(Expression metadata)
    | Variable !metadata !Name
    deriving (Eq, Functor, Show)

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

data Associativity = LeftAssoc | RightAssoc deriving (Eq, Show)

type Precedence = Int

data Operator = Operator
    { associativity :: !Associativity
    , precedence    :: !Precedence
    } deriving (Eq, Show)

parseFunction :: Text -> Either Text (Function ())
parseFunction = parse' function

function :: Parser (Function ())
function = label "function declaration" (try function' <|> functionSugar)

function' :: Parser (Function ())
function' = do
    type' <- typeName
    name <- variableName
    args <- tuple variableName
    body <- brackets ast
    pure $ Function name type' args body

functionSugar :: Parser (Function ())
functionSugar = do
    name <- variableName
    (types, args) <- unzip <$> tuple (liftA2 (,) typeName variableName)
    symbol "->"
    ret <- typeName
    body <- brackets ast
    pure $ Function name (FunctionType types ret) args body

parseAst :: Text -> Either Text [AST ()]
parseAst = parse' ast

ast :: Parser [AST ()]
ast = many $ choice [assign, try expression, if', match, return', while, var]

assign :: Parser (AST ())
assign = do
    left <- try $ leftOfAssignment <* void (lexeme (char '='))
    right <- expression0
    endl
    pure $ Assign left right
  where
    leftOfAssignment = secondary =<< variable

expression :: Parser (AST ())
expression = Expression <$> (expression0 <* endl)

if' :: Parser (AST ())
if' = do
    symbol "if"
    cond <- expression0
    trueBranch <- brackets ast
    falseBranch <- option [] (symbol "else" *> (brackets ast <|> pure <$> if'))
    pure $ If cond trueBranch falseBranch

match :: Parser (AST ())
match = do
    symbol "match"
    cond <- expression0
    pats <- brackets $ flip sepEndBy (lexeme (char ',')) $ liftA2 Branch pattern (brackets ast)
    pure $ Match cond pats

return' :: Parser (AST ())
return' = Return <$> (symbol "return" *> optional expression0 <* endl)

var :: Parser (AST ())
var = do
    type' <- typeName
    name <- variableName
    void (lexeme (char '='))
    value' <- expression0
    endl
    pure $ Var name type' value'

while :: Parser (AST ())
while = do
    symbol "while"
    cond <- expression0
    loop <- brackets ast
    pure $ While cond loop

pattern :: Parser MatchPattern
pattern = choice [structureP, literalP, nameP]
  where
    literalP = LiteralPattern <$> literal
    nameP = NamePattern <$> variableName
    structureP = StructurePattern <$> structure pattern

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
value = Structure () <$> structure expression0 <|> Literal () <$> literal <|> variable

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
