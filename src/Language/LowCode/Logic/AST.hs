module Language.LowCode.Logic.AST
    ( module Language.Common
    , VariableType (..)
    , AST (..)
    , Expression (..)
    , parseExpression
    ) where

import           Universum hiding (bool, map, take, takeWhile)
import qualified Universum.Unsafe as Unsafe

import           Data.Aeson
import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Text
import           Data.Char (isAlphaNum, toLower)
import           Data.Data
import qualified Data.Map.Strict as Map
import           Data.Text (cons, map)

import Language.Codegen
import Language.Common
import Language.Emit

data VariableType
    = BoolTy Bool
    | DoubleTy Double
    | IntegerTy Integer
    | TextTy Text
    deriving (Data, Eq, Show, Typeable)

instance FromJSON VariableType where
    parseJSON = withObject "variable" $ \o -> do
        tag    <- o .: "type"
        value' <- o .: "value"
        case tag of
            "bool"    -> BoolTy    <$> parseJSON value'
            "double"  -> DoubleTy  <$> parseJSON value'
            "integer" -> IntegerTy <$> parseJSON value'
            "text"    -> TextTy    <$> parseJSON value'
            other     -> fail $ "Expected 'bool', 'double', 'integer' or 'text', but got '" <> other <> "'."

instance ToJSON VariableType where
    toJSON (BoolTy    b) = object [ "bool"    .= toJSON b ]
    toJSON (DoubleTy  d) = object [ "double"  .= toJSON d ]
    toJSON (IntegerTy i) = object [ "integer" .= toJSON i ]
    toJSON (TextTy    t) = object [ "text"    .= toJSON t ]

data AST
    -- | Assigns a new value to the variable with the specified name and type
    -- and what follows after.
    = Assign Text Expression AST
    -- | Represents the end of a cycle, and optionally goes to a 'Start' node
    -- with the specified name.
    | End (Maybe Text)
    -- | Allows a condition to be tested, and contains the true branch, false
    -- branch and what follows after the branches.
    | If Expression AST AST AST
    -- | Logs a message to the terminal, followed by the remainder of the cycle.
    | Print Expression AST
    -- | Represents the start of a cycle with a given name.
    | Start Text AST
    -- | Declares a variable with the specified name and type, followed by the
    -- remainder of the cycle.
    | Var Text Expression AST
    -- | Represents a condition which should be run while a predicate is not
    -- met, followed by the remainder of the cycle.
    | While Expression AST AST
    deriving (Eq, Show)

instance FromJSON AST where
    parseJSON = withObject "logic ast" $ \o -> o .: "tag" >>= \case
        "assign" -> Assign <$> o .:  "variable"
                           <*> o .:  "expression"
                           <*> o .:  "next-ast"
        "end"    -> End    <$> o .:? "next-ast"
        "if"     -> If     <$> o .:  "expression"
                           <*> o .:  "true-branch-ast"
                           <*> o .:  "false-branch-ast"
                           <*> o .:  "next-ast"
        "print"  -> Print  <$> o .:  "expression"
                           <*> o .:  "next-ast"
        "start"  -> Start  <$> o .:  "name"
                           <*> o .:  "next-ast"
        "var"    -> Var    <$> o .:  "variable"
                           <*> o .:  "expression"
                           <*> o .:  "next-ast"
        "while"  -> While  <$> o .:  "expression"
                           <*> o .:  "while-ast"
                           <*> o .:  "next-ast"
        other    -> fail $ "Unknown tag '"
                           <> other
                           <> "'. Available tags are: 'assign', 'end', 'if'"
                           <> ", 'print', 'var' and 'while'."

instance ToJSON AST where
    toJSON = \case
        Assign variable expression ast -> object
            [ "tag"              .= String "assign"
            , "variable"         .= String variable
            , "expression"       .= toJSON expression
            , "next-ast"         .= toJSON ast
            ]
        End ast -> object
            [ "tag"              .= String "end"
            , "next-ast"         .= toJSON ast
            ]
        If expression true false ast -> object
            [ "tag"              .= String "if"
            , "expression"       .= toJSON expression
            , "true-branch-ast"  .= toJSON true
            , "false-branch-ast" .= toJSON false
            , "next-ast"         .= toJSON ast
            ]
        Print expression ast -> object
            [ "tag"              .= String "print"
            , "expression"       .= toJSON expression
            , "next-ast"         .= toJSON ast
            ]
        Start name ast -> object
            [ "tag"              .= String "start"
            , "name"             .= String name
            , "next-ast"         .= toJSON ast
            ]
        Var variable expression ast -> object
            [ "tag"              .= String "var"
            , "variable"         .= String variable
            , "expression"       .= toJSON expression
            , "next-ast"         .= toJSON ast
            ]
        While expression body ast -> object
            [ "tag"              .= String "while"
            , "expression"       .= toJSON expression
            , "while-ast"        .= toJSON body
            , "next-ast"         .= toJSON ast
            ]

data Expression
    = BinaryOp Expression Symbol Expression
    | Parenthesis Expression
    | UnaryOp Symbol Expression
    | Value (ValueType VariableType)
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
            , pure $ emit $ binarySymbolToText Map.! symbol'
            , codegen right
            ]
        Parenthesis expression -> mconcat <$> sequence
            [ pure $ emit "("
            , codegen expression
            , pure $ emit ")"
            ]
        UnaryOp symbol' expression -> mconcat <$> sequence
            [ pure $ emit $ unarySymbolToText Map.! symbol'
            , codegen expression
            ]
        Value value' -> pure $ case value' of
            Variable v -> emit v
            Constant c -> case c of
                BoolTy b -> emit $ show b
                DoubleTy d -> emit $ show d
                IntegerTy i -> emit $ show i
                TextTy t -> emit "\"" <> emit t <> emit "\""

data Arity = Unary | Binary deriving (Eq, Show)
data Associativity = LeftAssoc | RightAssoc deriving (Eq, Show)
type Precedence = Int

data Operator = Operator
    { arity         :: !Arity
    , associativity :: !Associativity
    , precedence    :: !Precedence
    , symbol        :: !Symbol
    } deriving (Eq, Show)

-- Reference:
-- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence
symbolToOperator :: Symbol -> Operator
symbolToOperator = \case
    Add          -> Operator Binary LeftAssoc  14 Add
    Divide       -> Operator Binary LeftAssoc  15 Divide
    Multiply     -> Operator Binary LeftAssoc  15 Multiply
    Negate       -> Operator Unary  RightAssoc 17 Negate
    Subtract     -> Operator Binary LeftAssoc  14 Subtract

    Different    -> Operator Binary LeftAssoc  11 Different
    Equal        -> Operator Binary LeftAssoc  11 Equal
    Greater      -> Operator Binary LeftAssoc  12 Greater
    GreaterEqual -> Operator Binary LeftAssoc  12 GreaterEqual
    Less         -> Operator Binary LeftAssoc  12 Less
    LessEqual    -> Operator Binary LeftAssoc  12 LessEqual

    And          -> Operator Binary LeftAssoc   6 And
    Not          -> Operator Unary  RightAssoc 17 Not
    Or           -> Operator Binary LeftAssoc   5 Or

parseExpression :: Text -> Either Text Expression
parseExpression = first toText . parseOnly expression0

-- Reference:
-- https://en.wikipedia.org/wiki/Operator-precedence_parser#Precedence_climbing_method
expression0 :: Parser Expression
expression0 = expression1 0 =<< nonBinary

expression1 :: Int -> Expression -> Parser Expression
expression1 minPrecedence lhs = maybe (pure lhs) (loop lhs) =<< peek
  where
    loop lhs' op
        | precedence op >= minPrecedence = do
            skipSpace
            void $ symbolToOperator <$> binaryOperator
            rhs <- nonBinary
            lookaheadMaybe <- peek
            case lookaheadMaybe of
                Nothing -> pure $ BinaryOp lhs' (symbol op) rhs
                Just lookahead -> do
                    (rhs', lookaheadMaybe') <- loop' rhs op lookahead
                    let result = BinaryOp lhs' (symbol op) rhs'
                    maybe (pure result) (loop result) lookaheadMaybe'
        | otherwise = pure lhs'

    loop' rhs op lookahead
        | (precedence lookahead > precedence op)
            || (precedence lookahead == precedence op && associativity lookahead == RightAssoc) = do
            rhs' <- expression1 (precedence lookahead) rhs
            skipSpace
            lookaheadMaybe <- peek
            maybe (pure (rhs', Nothing)) (loop' rhs' op) lookaheadMaybe
        | otherwise = pure (rhs, Just lookahead)

    peek = optional $ lookAhead $ symbolToOperator <$> binaryOperator

unary :: Parser Expression
unary = do
    op <- unaryOperator
    expr <- nonBinary
    pure $ UnaryOp op expr

nonBinary :: Parser Expression
nonBinary = skipSpace *> choice [parenthesis, unary, value] <* skipSpace

value :: Parser Expression
value = Value <$> (constant <|> variable)
  where
    constant = Constant <$> (    (IntegerTy <$> (signed decimal))
                             <|> (DoubleTy  <$> double)
                             <|> (BoolTy    <$> boolCI)
                             <|> (TextTy    <$> text))
    variable = Variable <$> variableName

between :: (Applicative f) => f left -> f middle -> f right -> f middle
between left middle right = left *> middle <* right

parenthesis :: Parser Expression
parenthesis = between (char '(') (Parenthesis <$> expression0) (char ')')

boolCI :: Parser Bool
boolCI = do
    b <- asciiCI "false" <|> asciiCI "true"
    let maybeB = toBool $ map toLower b
    case maybeB of
        Nothing -> fail $
            "Could not read '"
            <> toString b
            <> "'. Perhaps you meant 'false' or 'true'?"
        Just b' -> pure b'
  where
    toBool "false" = Just False
    toBool "true"  = Just True
    toBool _       = Nothing

text :: Parser Text
text = between (char '"') (takeWhile ((/=) '"')) (char '"')

variableName :: Parser Text
variableName = do
    head' <- letter <|> char '_'
    tail' <- takeWhile (\c -> isAlphaNum c || c == '_')
    pure $ cons head' tail'

unaryOperator :: Parser Symbol
unaryOperator = do
    symbol' <- choice (string <$> Map.elems unarySymbolToText)
    case Map.lookup symbol' unaryTextToSymbol of
        Nothing -> fail $
            "Undefined unary operator '"
            <> toString symbol'
            <> "'. This error should not appear and is likely a bug. "
            <> "Please report it."
        Just op -> pure op

binaryOperator :: Parser Symbol
binaryOperator = do
    symbol' <- choice (string <$> Map.elems binarySymbolToText)
    case Map.lookup symbol' binaryTextToSymbol of
        Nothing -> fail $
            "Undefined binary operator '"
            <> toString symbol'
            <> "'. This error should not appear and is likely a bug. "
            <> "Please report it."
        Just op -> pure op

unarySymbolToText :: Map Symbol Text
unarySymbolToText = Map.fromList
    [ (Negate, "-"  )
    , (Not   , "not")
    ]

unaryTextToSymbol :: Map Text Symbol
unaryTextToSymbol = Map.fromAscList $
    zip (Map.elems unarySymbolToText) (Map.keys unarySymbolToText)

binarySymbolToText :: Map Symbol Text
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

binaryTextToSymbol :: Map Text Symbol
binaryTextToSymbol = Map.fromAscList $
   zip (Map.elems binarySymbolToText) (Map.keys binarySymbolToText)
