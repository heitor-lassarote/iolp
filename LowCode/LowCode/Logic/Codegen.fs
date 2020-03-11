module LowCode.Logic.Codegen

open System

open AST

type Options =
    { BracesOnNewLine : bool
    ; CompactCode     : bool
    ; IndentLevel     : int
    }
with
    interface LowCode.Codegen.IOptions

module Options =
    let unit =
        { BracesOnNewLine = false
        ; CompactCode     = false
        ; IndentLevel     = 4
        }

type GeneratorState =
    { CurrentIndentLevel : int
    ; Options : Options
    }
with
    interface LowCode.Codegen.IGeneratorState with
        member this.Options = this.Options :> LowCode.Codegen.IOptions

module GeneratorState =
    let unit =
        { CurrentIndentLevel = 0
        ; Options = Options.unit
        }

let nl = System.Environment.NewLine

let indent generatorState =
    let spaces = String(' ', generatorState.CurrentIndentLevel)
    in
    if generatorState.Options.BracesOnNewLine
    then nl + spaces + "{" + nl + spaces
    else nl + spaces

let genFunction generatorState name args body =
    let thisIndent = generatorState.CurrentIndentLevel
    let nextIndent = thisIndent + generatorState.Options.IndentLevel
    let gen = { generatorState with CurrentIndentLevel = thisIndent }
    let i = indent gen
    in gen, sprintf "%sfunction %s(%s) {\n%s}" i name args body

let codegen generatorState =
    let rec codegen symbols =
        function
        | Start l ->
            //let gen, func = genFunction generatorState "TODO_addFunctionNames" "" (codegen generatorState symbols l)
            //in func
            sprintf "function TODO_addFunctionNames() {\n%s}" (codegen symbols l)
        | Var ((k, v), l) ->
            let symbols = Map.add k v symbols
            in sprintf "var %s = %s;\n%s" k (v.ToString()) (codegen symbols l)
        | If (p, t, f) ->
            let codegen = codegen symbols
            let get =
                function
                | Variable s -> s.ToString()
                | Constant c -> c.ToString()
            let expr =
                match p with
                | IsEqual (a, b) -> sprintf "%s === %s" (get a) (get b)
                | IsDifferent (a, b) -> sprintf "%s !== %s" (get a) (get b)
                | IsGreaterThan (a, b) -> sprintf "%s > %s" (get a) (get b)
                | IsLessThan (a, b) -> sprintf "%s < %s" (get a) (get b)
                | IsGreaterOrEqualTo (a, b) -> sprintf "%s >= %s" (get a) (get b)
                | IsLessOrEqualTo (a, b) -> sprintf "%s <= %s" (get a) (get b)
            in sprintf "if (%s)\n{\n%s}\nelse {\n%s}" expr (codegen t) (codegen f)
        | Print (s, l) ->
            sprintf "alert(\"%s\");\n%s" s (codegen symbols l)
        | End -> "return;\n"
    in Ok << codegen Map.empty

type JavaScriptCodegen<'a when 'a : comparison>() =
    interface LowCode.Codegen.ICodegen<AST<'a>> with
        member this.Codegen generator ast =
            codegen generator ast
