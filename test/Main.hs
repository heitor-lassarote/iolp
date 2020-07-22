module Main where

import           Universum

import           Data.Default.Class
import qualified Data.Map.Strict as Map

import           Language.Bundles
import           Language.Common
import qualified Language.CSS as CSS
import qualified Language.HTML as HTML
import qualified Language.LowCode.Logic as L

{- Test 1: Counter
    This test should generate a small page contained a button on the left, a
button on the right and a label between them, initialized with "0". The left
button should say "-" and the right button should say "+". Every time the left
buttin is pressed, the label should decrement its value, while the right button
should increment the value. Aditionally, to the right of the "+" button, there
should be a button which says "Reset", which sets the counter back to its
initial state when pressed.
-}
counterHtml :: [HTML.AST]
counterHtml =
    [ HTML.Tag "div" []
        [ HTML.Tag "button" [("type", "button"), ("onClick", "updateLabel(-1)")]
            [ HTML.Text "-"
            ]
        , HTML.Tag "p" [("id", "counter")]
            [ HTML.Text "0"
            ]
        , HTML.Tag "button" [("type", "button"), ("onClick", "updateLabel(1)")]
            [ HTML.Text "+"
            ]
        , HTML.Tag "button" [("type", "button"), ("onClick", "resetLabel()")]
            [ HTML.Text "Reset"
            ]
        ]
    ]

counterCss :: CSS.AST
counterCss = CSS.CSS []

element :: L.VariableType
element = L.RecordType
    [ ("innerHTML", L.TextType)
    ]

document :: L.VariableType
document = L.RecordType
    [ ("getElementById", L.FunctionType [L.TextType] element)
    ]

counterExterns :: Map Name L.VariableType
counterExterns = Map.fromList
    [ ("printInt", L.FunctionType [L.IntegerType] L.TextType)
    , ("parseInt", L.FunctionType [L.TextType, L.IntegerType] L.IntegerType)
    , ("element", element)
    , ("document", document)
    ]

counterEnvironment :: L.Environment
counterEnvironment = L.Environment
    { externs         = counterExterns
    , recordTemplates = Map.empty
    }

counterLogic :: [L.AST]
counterLogic =
    [ L.Start m "updateLabel" (L.FunctionType [L.IntegerType] L.UnitType) ["increment"] $
        L.Assign m access count $
        L.End
    , L.Start m "resetLabel" (L.FunctionType [] L.UnitType) [] $
        L.Assign m access (L.Value (Constant $ L.Text "0")) $
        L.End
    ]
  where
    m = L.Metadata (0, 0)
    Right access = L.parseExpression "document.getElementById(\"counter\").innerHTML"
    Right count = L.parseExpression "printInt(parseInt(document.getElementById(\"counter\").innerHTML, 10) + increment)"

counter :: BundleCssHtmlLogic
counter = BundleCssHtmlLogic
    { cssOptions       = def
    , extraJsFiles     = [("printInt.js", "printInt = x => x.toString();")]
    , htmlOptions      = def
    , jsOptions        = def
    , logicEnvironment = counterEnvironment
    , pages            =
        [ PageCssHtmlLogic
            { css   = counterCss
            , html  = counterHtml
            , logic = counterLogic
            , name  = "counter"
            }
        ]
    }

main :: IO ()
main = do
    print (generate counter :: Status Text)
