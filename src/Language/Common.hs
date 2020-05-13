module Language.Common where

import Universum

type Variable varType = (Text, varType)

data ValueType varType
    = Variable Text
    | Constant varType
    deriving (Functor)

type Comparison varType = (ValueType varType, EqualityOp, ValueType varType)

data EqualityOp
    = IsEqual
    | IsDifferent
    | IsGreaterThan
    | IsLessThan
    | IsGreaterOrEqualTo
    | IsLessOrEqualTo
