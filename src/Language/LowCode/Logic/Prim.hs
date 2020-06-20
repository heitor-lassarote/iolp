module Language.LowCode.Logic.Prim where

import Universum

import           Language.Common
import qualified Language.LowCode.Logic.Types as L

isNumeric :: L.VariableType -> Bool
isNumeric = \case
    L.DoubleType  -> True
    L.IntegerType -> True
    _             -> False

isText :: L.VariableType -> Bool
isText = \case
    L.CharType -> True
    L.TextType -> True
    _          -> False

vtType :: L.Variable -> L.VariableType
vtType = \case
    L.Bool _    -> L.BoolType
    L.Char _    -> L.CharType
    L.Double _  -> L.DoubleType
    L.Integer _ -> L.IntegerType
    L.Text _    -> L.TextType
    L.Unit      -> L.UnitType

interactsWithUnary :: UnarySymbol -> L.VariableType -> Maybe L.VariableType
interactsWithUnary Negate = \case
    L.DoubleType  -> Just L.DoubleType
    L.IntegerType -> Just L.IntegerType
    _             -> Nothing
interactsWithUnary Not = \case
    L.BoolType    -> Just L.BoolType
    L.IntegerType -> Just L.IntegerType
    _             -> Nothing

interactsWithBinary :: L.VariableType -> BinarySymbol -> L.VariableType -> Maybe L.VariableType
interactsWithBinary L.TextType Add L.TextType = Just L.TextType  -- Concatenate strings.
interactsWithBinary L.BoolType s L.BoolType
    | isLogical s = Just L.BoolType  -- Logical operators only interact with logical variables.
    | otherwise   = Nothing
interactsWithBinary l s r
    | l /= r                        = Nothing  -- Different types never interact.
    | isArithmetic s && isNumeric l = Just l  -- Numeric types interact with all arithmetic operators.
    | isComparison s                = Just L.BoolType  -- Comparisons always return booleans.
    | otherwise                     = Nothing
