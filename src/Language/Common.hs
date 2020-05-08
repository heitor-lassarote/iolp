module Language.Common where

import Data.Text

type Variable varType = (Text, varType)

data ValueType varType
    = Variable Text
    | Constant varType
    deriving (Functor)

data Comparison varType
    = IsEqual (ValueType varType) (ValueType varType)
    | IsDifferent (ValueType varType) (ValueType varType)
    | IsGreaterThan (ValueType varType) (ValueType varType)
    | IsLessThan (ValueType varType) (ValueType varType)
    | IsGreaterOrEqualTo (ValueType varType) (ValueType varType)
    | IsLessOrEqualTo (ValueType varType) (ValueType varType)
    deriving (Functor)
