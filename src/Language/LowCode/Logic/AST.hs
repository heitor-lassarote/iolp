module Language.LowCode.Logic.AST
    ( module Language.Common
    , VariableType (..)
    , AST (..)
    , Expression (..)
    , parseExpression
    ) where

import           Universum hiding (bool, map, take, takeWhile)
import qualified Universum.Unsafe as Unsafe

import           Control.Monad.Trans.Except (throwE)
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
    = BinaryOp Expression BinarySymbol Expression
    | Parenthesis Expression
    | UnaryOp UnarySymbol Expression
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
            , tryBinaryText symbol'
            , codegen right
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
        Value value' -> pure $ case value' of
            Variable v -> emit v
            Constant c -> case c of
                BoolTy b -> emit $ show b
                DoubleTy d -> emit $ show d
                IntegerTy i -> emit $ show i
                TextTy t -> emit "\"" <> emit t <> emit "\""
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
            skipSpace
            lookaheadMaybe <- peek
            maybe (pure (rhs', Nothing)) (loop' rhs' op) lookaheadMaybe
        | otherwise = pure (rhs, Just lookahead)

    peek = optional $ lookAhead $ binarySymbolToOperator <$> binaryOperator

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
biject m = Map.fromList $ zip (Map.elems m) (Map.keys m)

unarySymbolToText :: Map UnarySymbol Text
unarySymbolToText = Map.fromList
    [ (Negate, "-"  )
    , (Not   , "not")
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
