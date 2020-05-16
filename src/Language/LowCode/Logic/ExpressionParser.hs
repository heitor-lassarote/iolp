module Language.LowCode.Logic.ExpressionParser where

import Universum hiding (bool, map, take, takeWhile)

import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Text
import           Data.Char (isAlphaNum, toLower)
import qualified Data.Map as M
import           Data.Text (cons, map)

import Language.Common
import Language.LowCode.Logic.AST (VariableType (..), Expression (..))

data Arity = Unary | Binary deriving (Eq, Show)
data Associativity = LeftAssoc | RightAssoc deriving (Eq, Show)
type Precedence = Int

data Operator = Operator
    { arity         :: {-# UNPACK #-} !Arity
    , associativity :: {-# UNPACK #-} !Associativity
    , precedence    :: {-# UNPACK #-} !Precedence
    , symbol        :: {-# UNPACK #-} !Symbol
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
parseExpression = first toText . parseOnly expression

-- Reference:
-- https://en.wikipedia.org/wiki/Operator-precedence_parser#Precedence_climbing_method
expression :: Parser Expression
expression = expression1 0 =<< nonBinary

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

unary' :: Parser Expression
unary' = do
    op <- unaryOperator
    expr <- nonBinary
    pure $ UnaryOp op expr

nonBinary :: Parser Expression
nonBinary = skipSpace *> choice [parenthesis, unary', value] <* skipSpace

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
parenthesis = between (char '(') (Parenthesis <$> expression) (char ')')

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
    symbol' <- choice (string <$> unaryOperatorsText)
    case M.lookup symbol' unaryMap of
        Nothing -> fail $
            "Undefined unary operator '"
            <> toString symbol'
            <> "'. This error should not appear and is likely a bug. "
            <> "Please report it."
        Just op -> pure op

binaryOperator :: Parser Symbol
binaryOperator = do
    symbol' <- choice (string <$> binaryOperatorsText)
    case M.lookup symbol' binaryMap of
        Nothing -> fail $
            "Undefined binary operator '"
            <> toString symbol'
            <> "'. This error should not appear and is likely a bug. "
            <> "Please report it."
        Just op -> pure op

unaryOperatorsText :: [Text]
unaryOperatorsText =
    [ "-"
    , "not"
    ]

binaryOperatorsText :: [Text]
binaryOperatorsText =
    [ "+"
    , "/"
    , "*"
    , "-"
    , "<>"
    , "="
    , ">"
    , ">="
    , "<"
    , "<="
    , "and"
    , "or"
    ]

unaryMap :: Map Text Symbol
unaryMap = M.fromList $ zip unaryOperatorsText
    [ Negate
    , Not
    ]

binaryMap :: Map Text Symbol
binaryMap = M.fromList $ zip binaryOperatorsText
    [ Add
    , Divide
    , Multiply
    , Subtract
    , Different
    , Equal
    , Greater
    , GreaterEqual
    , Less
    , LessEqual
    , And
    , Or
    ]
