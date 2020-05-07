module Main where

import Language.LowCode.Codegen as C
import Language.LowCode.Logic as L
import Language.LowCode.UI as UI

prototypeLogic :: L.LogicAST
prototypeLogic =
    L.Start $
        L.Var ("x", L.IntegerTy 5) $
            L.If (L.IsEqual (L.Constant $ L.IntegerTy 5) (L.Variable "x"))
                 (L.Print "Equal" L.End)
                 (L.Print "Different" L.End)
                 L.End

prototypeHtml :: UI.HtmlAST
prototypeHtml = UI.Tag "div" []
    [ UI.Tag "h1" [] [UI.Text "Hello, HTML!"]
    , UI.Tag "p" [("title", "And I'm a tooltip!")] [UI.Text "This is my first paragraph. :)"]
    , UI.Tag "script" [] [either UI.Text UI.Text $ C.codegen prototypeLogic]
    ]

main :: IO ()
main =
    let html = C.codegen prototypeHtml
     in case html of
        Left e -> print e
        Right code -> do
            putStrLn code
            writeFile "/home/heitor/prototype.html" code

