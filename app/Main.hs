module Main where

import Data.Text

import Language.Codegen
import Language.HTML as HTML
import Language.JavaScript as JS
import Language.LanguageConverter
import Language.LowCode.Logic as L
import Language.LowCode.UI as UI

prototypeLogic :: JS.AST
prototypeLogic = convert $
    L.Start $
        L.Var "x" (L.IntegerTy 5) $
            L.If (L.IsEqual (L.Constant $ L.IntegerTy 5) (L.Variable "x"))
                 (L.Print "Equal" L.End)
                 (L.Print "Different" L.End)
                 L.End

prototypeHtml :: HTML.AST
prototypeHtml = convert $
    UI.Tag "div" []
        [ UI.Tag "h1" [] [UI.Text "Hello, HTML!"]
        , UI.Tag "p" [("title", "And I'm a tooltip!")] [UI.Text "This is my first paragraph. :)"]
        , UI.Tag "script" [] [either UI.Text UI.Text $ codegen $ prototypeLogic]
        ]

main :: IO ()
main =
    let html = codegen prototypeHtml
     in case html of
        Left e -> print e
        Right code -> do
            putStrLn code
            writeFile "/home/heitor/prototype.html" code

