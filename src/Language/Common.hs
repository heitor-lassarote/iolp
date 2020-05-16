module Language.Common where

import Universum

type Variable varType = (Text, varType)

data ValueType varType
    = Variable Text
    | Constant varType
    deriving (Eq, Functor, Show)

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
    deriving (Eq, Show)
