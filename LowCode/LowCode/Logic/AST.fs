module LowCode.Logic.AST

open System

open LowCode.Utility

type Variable<'a> = string * 'a

type ValueType<'a> =
    | Variable of string
    | Constant of 'a

type Comparison<'a, 'b> =
    | IsEqual of ValueType<'a> * ValueType<'b>
    | IsDifferent of ValueType<'a> * ValueType<'b>
    | IsGreaterThan of ValueType<'a> * ValueType<'b>
    | IsLessThan of ValueType<'a> * ValueType<'b>
    | IsGreaterOrEqualTo of ValueType<'a> * ValueType<'b>
    | IsLessOrEqualTo of ValueType<'a> * ValueType<'b>

type AST<'a when 'a : comparison> =
    | Start of AST<'a>
    | Var of Variable<'a> * AST<'a>
    | If of Comparison<'a, 'a> * AST<'a> * AST<'a>
    | Print of string * AST<'a>
    | End
with
    interface LowCode.AST.IAST

let start = Start
let var<'a when 'a : comparison> = curry Var
let if'<'a when 'a : comparison> = curry2 If
let print<'a when 'a : comparison> = curry Print
let end' = End
