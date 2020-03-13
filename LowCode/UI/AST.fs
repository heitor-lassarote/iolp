module LowCode.UI.AST

open LowCode.Utility

type Attribute = string * string
type Attributes = Attribute list

type AST<'a when 'a : comparison> =
    | HTML of AST<'a>
    | Body of AST<'a> list
    | H1 of Attributes * string
    | P of Attributes * string
    | Script of LowCode.Logic.AST.AST<'a>
    | Text of string
with
    interface LowCode.AST.IAST
    
let html = HTML
let body = Body
let h1<'a when 'a : comparison> = curry H1
let p<'a when 'a : comparison> = curry P
let script = Script
let text = Text
