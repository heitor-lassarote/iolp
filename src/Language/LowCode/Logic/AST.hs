module Language.LowCode.Logic.AST
    ( module Language.Common
    , VariableType (..)
    , AST (..)
    , Expression (..)
    ) where

import Universum

import Language.Common

data VariableType
    = BoolTy Bool
    | DoubleTy Double
    | IntegerTy Integer
    | TextTy Text
    deriving (Eq, Show)

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

data Expression
    = BinaryOp Expression Symbol Expression
    | Parenthesis Expression
    | UnaryOp Symbol Expression
    | Value (ValueType VariableType)
    deriving (Eq, Show)

data ExpressionType
    = OperatorTy Symbol
    | ValueTy (ValueType VariableType)
    deriving (Eq, Show)
