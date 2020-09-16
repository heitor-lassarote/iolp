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
    [ L.Field "getText" (L.FunctionType [] L.TextType)
    , L.Field "setText" (L.FunctionType [L.TextType] L.unitType)
    ]

counterExterns :: Map Name L.Type
counterExterns = Map.fromList
    [ ("printInt", L.FunctionType [L.IntegerType] L.TextType)
    , ("parseInt", L.FunctionType [L.TextType, L.IntegerType] L.IntegerType)
    , ("element", element)
    , ("getElement", L.FunctionType [L.TextType] element)
    ]

counterLogic :: [L.Module () L.Metadata]
counterLogic =
    [ L.Module
        { adtTemplates = Map.empty
        , externs = counterExterns
        , functions =
            [ L.Function m "updateLabel" (L.FunctionType [L.IntegerType] L.unitType) ["increment"]
              $ L.Var m "counter" element getElement
              $ L.Var m "count" L.IntegerType count
              $ L.Expression m setCount
              $ L.End m
            , L.Function m "resetLabel" (L.FunctionType [] L.unitType) []
              $ L.Var m "counter" element getElement
              $ L.Expression m resetCount
              $ L.End m
            ]
        , importedModules = []
        , moduleName = "Counter"
        , recordTemplates = Map.empty
        }
    ]
  where
    m = L.Metadata (0, 0)
    Right getElement = L.parseExpression "getElement(\"counter\")"
    Right count = L.parseExpression "parseInt(counter.getText(), 10) + increment"
    Right setCount = L.parseExpression "counter.setText(printInt(count))"
    Right resetCount = L.parseExpression "counter.setText(\"0\")"

counter :: BundleCssHtmlLogic
counter = BundleCssHtmlLogic
    { cssOptions       = def
    , extraJsFiles     = [("printInt.js", "printInt = x => x.toString();")]
    , htmlOptions      = def
    , jsOptions        = def
    , mainFunction     = (L.mkModule "main") {L.importedModules = ["Counter"]}
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
    print (generate counter :: Status [(FilePath, Text)] [(FilePath, Text)])
