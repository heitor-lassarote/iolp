module LowCode.Prototype

open Logic.AST
open Logic.Codegen

open UI.AST
open UI.Codegen

let prototype =
    let logic =
        start
            ( var ("x", 5 :> System.IComparable)
                ( if'
                    (IsEqual (Constant (upcast 5), Variable "x"))
                    (print "is equal" end')
                    (print "is different" end')
                )
            )
    let uiGen = LowCode.UI.Codegen.GeneratorState.unit
    let uiGen = { uiGen with Options = { uiGen.Options with ScriptGenerator = Some (upcast JavaScriptCodegen()) } }
    let ui =
        html
            ( body
                [ h1 [] "Hello, HTML!"
                ; p [("title", "And I'm a tooltip!")]
                    "This is my first paragraph. :)"
                ; script logic
                ]
            )
        |> codegen uiGen
    in System.IO.File.WriteAllText("prototype.html", ui) |> ignore

[<EntryPoint>]
let main argv =
    prototype |> ignore
    0
