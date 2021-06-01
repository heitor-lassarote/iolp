module Language.LowCode.Logic.Standard
    ( module Language.LowCode.Logic.Standard.JSON
    , module Language.LowCode.Logic.Standard.Prelude
    , module Language.LowCode.Logic.Standard.REST
    , standardModules
    ) where

import Universum

import qualified Data.Map.Strict as Map

import Language.Common
import Language.LowCode.Logic.Module
import Language.LowCode.Logic.Standard.JSON
import Language.LowCode.Logic.Standard.Prelude
import Language.LowCode.Logic.Standard.REST

-- | These modules are expected to always be found by the user when trying to
-- include them.
standardModules :: Map Name (Module ())
standardModules = Map.fromList $ map (moduleName &&& id)
    [ moduleJson
    , modulePrelude
    , moduleRest
    ]
