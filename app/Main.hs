module Main where

import Data.Text

import Language.Codegen
import Language.HTML as H
import Language.HTML.Attributes as HA
import Language.HTML.Elements as HE
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

prototypeHtml :: H.AST
prototypeHtml = convert $
    HE.div'
        [ HE.h1' [H.text "Hello, HTML!"]
        , HE.p [HA.title "And I'm a tooltip!"] [H.text "This is my first paragraph. :)"]
        , HE.script' [either H.text H.text $ codegen prototypeLogic]
        ]

main :: IO ()
main =
    let html = codegen prototypeHtml
     in case html of
        Left e -> print e
        Right code -> do
            putStrLn code
            writeFile "/home/heitor/prototype.html" code
