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

element :: L.Type
element = L.RecordType
    [ L.Field "innerHTML" L.TextType
    ]

document :: L.Type
document = L.RecordType
    [ L.Field "getElementById" (L.FunctionType [L.TextType] element)
    ]

counterExterns :: Map Name L.Type
counterExterns = Map.fromList
    [ ("intToString", L.FunctionType [L.IntegerType] L.TextType)
    , ("parseInt", L.FunctionType [L.TextType, L.IntegerType] L.IntegerType)
    , ("element", element)
    , ("document", document)
    ]

counterEnvironment :: L.Environment
counterEnvironment = L.Environment
    { externs         = counterExterns
    , recordTemplates = Map.empty
    }

counterLogic :: [L.AST () L.Metadata]
counterLogic =
    [ L.Start m "updateLabel" (L.FunctionType [L.IntegerType] L.unitType) ["increment"] $
        L.Var m "element" L.TextType element $
        L.Var m "counter" L.IntegerType count $
        L.Expression m setCount
        L.End
    , L.Start m "resetLabel" (L.FunctionType [] L.unitType) [] $
        L.Expression m resetCount
        L.End
    ]
  where
    m = L.Metadata (0, 0)
    Right element = L.parseExpression "getElement(\"counter\")"
    Right count = L.parseExpression "stringToInt(element.getText(), 10) + increment"
    Right setCount = L.parseExpression "element.setText(intToString(counter))"
    Right resetCount = L.parseExpression "element.setText(0)"

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
