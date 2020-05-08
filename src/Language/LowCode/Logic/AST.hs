module Language.LowCode.Logic.AST
    ( module Language.Common
    , VariableType (..)
    , AST (..)
    ) where

import Data.Text

import Language.Common

data VariableType
    = FloatTy Float
    | IntegerTy Integer
    | TextTy Text
    deriving (Eq, Show)

data AST
    = Start AST
    | Var Text VariableType AST
    | If (Comparison VariableType) AST AST AST
    | Print Text AST
    | End
