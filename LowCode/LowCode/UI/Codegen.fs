module LowCode.UI.Codegen

open System

open AST

open LowCode.AST
open LowCode.Codegen

type Options<'ast when 'ast :> IAST> =
    { CompactCode     : bool
    ; IndentLevel     : int
    ; ScriptGenerator : ICodegen<'ast> option
    }

module Options =
    let unit =
        { CompactCode     = false
        ; IndentLevel     = 2
        ; ScriptGenerator = None
        }
    
type GeneratorState<'ast when 'ast :> IAST> =
    { CurrentIndentLevel : int
    ; Options : Options<'ast>
    }

module GeneratorState =
    let unit =
        { CurrentIndentLevel = 0
        ; Options = Options.unit
        }

let nl = System.Environment.NewLine

let indent value = nl + String(' ', value)

let rec attributes : Attributes -> string =
    function
    | (name, value)::xs -> sprintf " %s=\"%s\"%s" name value (attributes xs)
    | []                -> ""

and elements generatorState =
    let opts = generatorState.Options
    let level = generatorState.CurrentIndentLevel
    let rec element =
        function
        | x::xs ->
            indent level + codegen generatorState x + elements generatorState xs
        | []    ->
            indent (level - opts.IndentLevel)
    in element
    
and tag generatorState name attrs elmnts =
    let opts = generatorState.Options
    let es = elements { generatorState with CurrentIndentLevel = generatorState.CurrentIndentLevel + opts.IndentLevel } elmnts
    in sprintf "<%s%s>%s</%s>" name (attributes attrs) es name
    
and codegen generatorState =
    let tag = tag generatorState
    let rec codegen =
        function
        | HTML ast -> tag "html" [] [ast]
        | Body asts -> tag "body" [] asts
        | H1 (attrs, str) -> tag "h1" attrs [text str]
        | P (attrs, str) -> tag "p" attrs [text str]
        | Script l ->
            match generatorState.Options.ScriptGenerator with
            | Some gen ->
                match LowCode.Logic.Codegen.codegen gen l with
                | Ok code -> tag "script" [] [text code]
                | Error error -> error
            | None -> tag "script" [] []
        | Text str -> str
    in codegen
