module Language.LowCode.Logic.Standard.Prelude
    ( modulePrelude
    , boolType
    , false
    , true
    , bool
    , unitType
    , unit'
    , unit
    ) where

import Universum hiding (Type, bool)

import qualified Data.Map.Strict as Map

import Language.LowCode.Logic.AST
import Language.LowCode.Logic.Module
import Language.LowCode.Logic.Structure
import Language.LowCode.Logic.Type

modulePrelude :: Module e
modulePrelude = Module
    { adtTemplates = Map.unions
        [ bool
        , unit
        ]
    , externs = Map.fromList
        [ ("doubleToInteger", FunctionType [DoubleType] IntegerType)
        , ("doubleToText", FunctionType [DoubleType] TextType)
        , ("integerToDouble", FunctionType [IntegerType] DoubleType)
        , ("integerToText", FunctionType [IntegerType] TextType)
        , ("textToDouble", FunctionType [TextType] DoubleType)
        , ("textToInteger", FunctionType [TextType] IntegerType)
        , ("textToIntegerRadix", FunctionType [TextType, IntegerType] IntegerType)
        , ("trunc", FunctionType [DoubleType] IntegerType)
        , ("event", FunctionType [TextType, TextType, FunctionType [] unitType] unitType)
        , ("handleHtmlElement", FunctionType [TextType, TextType] unitType)
        ]
    , functions = []
    , importedModules = []
    , moduleName = "Prelude"
    }

-- TODO: In the future, move these "concrete" types to a standard library.
boolType :: Type
boolType = AlgebraicType "Bool"

falseConstructor :: Constructor e
falseConstructor = Constructor "Bool" "False" Nothing

false :: Structure e
false = Algebraic falseConstructor

trueConstructor :: Constructor e
trueConstructor = Constructor "Bool" "True" Nothing

true :: Structure e
true = Algebraic trueConstructor

bool :: Map Name [Constructor e]
bool = Map.singleton "Bool" [falseConstructor, trueConstructor]

unitType :: Type
unitType = AlgebraicType "Unit"

unitConstructor :: Constructor e
unitConstructor = Constructor "Unit" "Unit" Nothing

unit' :: Structure e
unit' = Algebraic unitConstructor

unit :: Map Name [Constructor e]
unit = Map.singleton "Unit" [unitConstructor]
