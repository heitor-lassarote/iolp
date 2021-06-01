module LogicModuleTests
    ( test
    ) where

import Universum

import           Data.Default.Class
import qualified Data.Map.Strict as Map
import           Data.These.Combinators
import           Test.Hspec

import Language.LowCode.Logic.AST
import Language.LowCode.Logic.Analyzer
import Language.LowCode.Logic.Module
import Language.LowCode.Logic.Standard

root :: Module () ()
root = (mkModule "Root")
    { functions = [Function () "RootFunction" (FunctionType [] DoubleType) [] $ Return () (Just $ Call () (Variable () "BarExtern") [Call () (Variable () "FooFunction") []])]
    , importedModules = ["Bool", "Foo", "Bar"]
    }

modules :: NonEmpty (Module () ())
modules = root :|
    [ (mkModule "Bool")
        { adtTemplates = Map.singleton "Bool"
            [ Constructor "True"  Nothing
            , Constructor "False" Nothing
            ]
        }
    , (mkModule "Foo")
        { functions = [Function () "FooFunction" (FunctionType [] boolType) [] $ Return () (Just $ Call () (Variable () "True") [])]
        , importedModules = ["Bool"]
        }
    , (mkModule "Bar")
        { externs = Map.singleton "BarExtern" (FunctionType [boolType] DoubleType)
        }
    ]

test :: IO ()
test = hspec do
    describe "Language.LowCode.Logic.Module" do
        it "adds imported modules scopes into the current module" do
            analyze modules `shouldSatisfy` isThat
