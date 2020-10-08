module Language.LowCode.Logic.Standard.Prelude
    ( prelude
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
import Language.LowCode.Logic.Type

prelude :: Module () ()
prelude = Module
    { adtTemplates = Map.unions
        [ bool
        , unit
        ]
    , externs = Map.empty
    , functions = []
    , importedModules = []
    , moduleName = "Prelude"
    }

-- TODO: In the future, move these "concrete" types to a standard library.
boolType :: Type
boolType = AlgebraicType "Bool"

falseConstructor :: Constructor e
falseConstructor = Constructor "Bool" "False" Nothing

false :: Structure Type
false = Algebraic boolType falseConstructor

trueConstructor :: Constructor e
trueConstructor = Constructor "Bool" "True" Nothing

true :: Structure Type
true = Algebraic boolType trueConstructor

bool :: Map Name [Constructor Type]
bool = Map.singleton "Bool" [falseConstructor, trueConstructor]

unitType :: Type
unitType = AlgebraicType "Unit"

unitConstructor :: Constructor e
unitConstructor = Constructor "Unit" "Unit" Nothing

unit' :: Structure Type
unit' = Algebraic unitType unitConstructor

unit :: Map Name [Constructor Type]
unit = Map.singleton "Unit" [unitConstructor]
