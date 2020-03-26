module LowCode.Logic.AST where

import Prelude

import Data.List as L

data VariableDeclaration a = VariableDeclaration
    { name  :: String
    , value :: a
    }

derive instance eqVariableDeclaration :: (Eq a) => Eq (VariableDeclaration a)

data ValueType a
    = Variable String
    | Constant a

derive instance eqValueType :: (Eq a) => Eq (ValueType a)

data Comparison a
    = IsEqual (ValueType a) (ValueType a)
    | IsDifferent (ValueType a) (ValueType a)
    | IsGreaterThan (ValueType a) (ValueType a)
    | IsLessThan (ValueType a) (ValueType a)
    | IsGreaterOrEqualTo (ValueType a) (ValueType a)
    | IsLessOrEqualTo (ValueType a) (ValueType a)

derive instance eqComparison :: (Eq a) => Eq (Comparison a)

data AST a
    = Start (L.List (AST a))
    | Var (VariableDeclaration a)
    | If (Comparison a) (L.List (AST a)) (L.List (AST a))
    | Print String
    | End

derive instance eqAST :: (Eq a) => Eq (AST a)

