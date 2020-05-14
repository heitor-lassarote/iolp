module Language.Common where

import Universum

type Variable varType = (Text, varType)

data ValueType varType
    = Variable Text
    | Constant varType
    deriving (Functor)

data Arity = Unary | Binary
type Precedence = Int

data Operator = Operator {-# UNPACK #-} !Arity
                         {-# UNPACK #-} !Precedence
                         {-# UNPACK #-} !Symbol

data Symbol
    -- Arithmetic
    = Add
    | Divide
    | Multiply
    | Negate
    | Subtract

    -- Comparison
    | Different
    | Equal
    | Greater
    | GreaterEqual
    | Less
    | LessEqual

    -- Logical
    | And
    | Not
    | Or

-- Reference:
-- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence
symbolToOperator :: Symbol -> Operator
symbolToOperator = \case
    Add -> Operator Binary 14 Add
    Divide -> Operator Binary 15 Divide
    Multiply -> Operator Binary 15 Multiply
    Negate -> Operator Unary 17 Negate
    Subtract -> Operator Binary 14 Subtract

    Different -> Operator Binary 11 Different
    Equal -> Operator Binary 11 Equal
    Greater -> Operator Binary 12 Greater
    GreaterEqual -> Operator Binary 12 GreaterEqual
    Less -> Operator Binary 12 Less
    LessEqual -> Operator Binary 12 LessEqual

    And -> Operator Binary 6 And
    Or -> Operator Binary 5 Or
