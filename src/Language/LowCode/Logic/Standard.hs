module Language.LowCode.Logic.Standard
    ( boolType
    , false
    , true
    , unitType
    , unit
    , prelude
    ) where

import Universum hiding (Type)

import qualified Data.Map.Strict as Map

import Language.LowCode.Logic.AST
import Language.LowCode.Logic.Module

-- TODO: In the future, move these "concrete" types to a standard library.
boolType :: Type
boolType = AlgebraicType "Bool"

falseConstructor :: Constructor e
falseConstructor = Constructor "False" Nothing

false :: Structure Type
false = Algebraic boolType falseConstructor 

trueConstructor :: Constructor e
trueConstructor = Constructor "True" Nothing

true :: Structure Type
true = Algebraic boolType trueConstructor

unitType :: Type
unitType = AlgebraicType "Unit"

unitConstructor :: Constructor e
unitConstructor = Constructor "Unit" Nothing

unit :: Structure Type
unit = Algebraic unitType unitConstructor

prelude :: Module Type a
prelude = Module
    { adtTemplates = Map.fromList
        [ ("Bool", [falseConstructor, trueConstructor])
        , ("Unit", [unitConstructor])
        ]
    , externs = Map.empty
    , functions = []
    , importedModules = []
    , moduleName = "Prelude"
    , recordTemplates = Map.empty
    }
