module LowCode.Codegen

open LowCode.AST

type Error = string

type IOptions = interface end

type IGeneratorState =
    abstract member Options : IOptions with get

type ICodegen<'ast when 'ast :> IAST> =
    abstract member Codegen : IGeneratorState -> 'ast -> Result<string, Error>
