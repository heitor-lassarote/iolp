-- There seems to be a bug in which something that uses convert makes GHC
-- complain about the function not existing when the module is not imported.
-- However, GHC thinks it's an empty export, and so explicitly disable these
-- warnings.
{-# OPTIONS_GHC -Wno-dodgy-exports  #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Language.LowCode.Logic
    ( module Language.LowCode.Logic.Analyzer
    , module Language.LowCode.Logic.AST
    , module Language.LowCode.Logic.Error
    , module Language.LowCode.Logic.JavaScriptConverter
    , module Language.LowCode.Logic.Module
    , module Language.LowCode.Logic.Standard
    , Metadata (..)
    ) where

import Universum

import Data.Aeson

import Language.LowCode.Logic.Analyzer
import Language.LowCode.Logic.AST
import Language.LowCode.Logic.Error
import Language.LowCode.Logic.JavaScriptConverter
import Language.LowCode.Logic.Module
import Language.LowCode.Logic.Standard

newtype Metadata = Metadata
    { position :: Double2
    } deriving stock    (Eq, Generic, Ord, Show)
      deriving anyclass (FromJSON, ToJSON)
