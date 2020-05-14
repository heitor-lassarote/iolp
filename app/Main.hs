module Main where

import Universum

import Language.Codegen
import Language.CSS as C
import Language.HTML as H
import Language.HTML.Attributes as HA
import Language.HTML.Elements as HE
import Language.JavaScript as JS
import Language.LanguageConverter
import Language.LowCode.Logic as L
import Language.LowCode.UI as UI

prototypeLogic :: JS.AST
prototypeLogic = convert $
    L.Start "test" $
    L.Var "x" (L.Value $ L.Constant $ L.IntegerTy 0) $
    L.While (L.BinaryOp (L.Value $ L.Variable "x") L.Less (L.Value $ L.Constant $ L.IntegerTy 5))
        (L.Print (L.Value $ L.Constant $ L.TextTy "x") $
        (L.Assign "x" (L.BinaryOp (L.Value $ L.Variable "x") L.Add (L.Value $ L.Constant $ L.IntegerTy 1)))
        (L.End Nothing)) $
    L.If (L.BinaryOp (L.Value $ L.Variable "x") L.Equal (L.Value $ L.Constant $ L.IntegerTy 5))
         (L.Print (L.Value $ L.Constant $ L.TextTy "\"Equal\"") $ L.End Nothing)
         (L.Print (L.Value $ L.Constant $ L.TextTy "\"Different\"") $ L.End Nothing)
         (L.End Nothing)

prototypeCss :: C.AST
prototypeCss =
    C.CSS
        [ C.Class "body"
            [ ("background", "black")
            , ("color", "white")
            ]
        ]

prototypeHtml :: H.AST
prototypeHtml = convert $
    H.Tag "html" []
        [ H.Tag "head" []
            [ H.Tag "style" [] [either H.text H.text $ evalCSS prototypeCss]
            ]
        , H.Tag "body" []
            [ HE.div'
                [ HE.h1' [H.text "Hello, HTML!"]
                , HE.p [HA.title "And I'm a tooltip!"] [H.text "This is my first paragraph. :)"]
                , HE.script' [either H.text H.text $ evalJS prototypeLogic]
                ]
            ]
        ]
  where
    evalCSS = evalCodegenT  C.defaultGeneratorState . codegen
    evalJS  = evalCodegenT JS.defaultGeneratorState . codegen

main :: IO ()
main =
    case evalCodegenT H.defaultGeneratorState $ codegen prototypeHtml of
        Left e -> print e
        Right code -> do
            putTextLn code
            writeFile "/home/heitor/prototype.html" code
