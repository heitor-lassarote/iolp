module Main where

import           Universum
import qualified Universum.Unsafe as Unsafe

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
    L.While whileCond
        (L.Print (L.Value $ L.Constant $ L.TextTy "x") $
        (L.Assign "x" xPlus1)
        (L.End Nothing)) $
    L.If ifCond
         (L.Print (L.Value $ L.Constant $ L.TextTy "\"Equal\"") $ L.End Nothing)
         (L.Print (L.Value $ L.Constant $ L.TextTy "\"Different\"") $ L.End Nothing) $
    L.Print formula (L.End Nothing)
  where
    whileCond = Unsafe.fromJust $ rightToMaybe $ L.parseExpression "x < 5"
    xPlus1 = Unsafe.fromJust $ rightToMaybe $ L.parseExpression "x + 1"
    ifCond = Unsafe.fromJust $ rightToMaybe $ L.parseExpression "x = 5"
    formula = Unsafe.fromJust $ rightToMaybe $ L.parseExpression "1 * (2 + x) * 4 + -2 - x"

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
