module Language.Bundles where

import Universum

import Data.Aeson

import qualified Language.CSS.AST as CSS
import qualified Language.LowCode.Logic as Logic
import qualified Language.LowCode.UI as UI

data BundleCssLogicUi = BundleCssLogicUi
    { css   :: [CSS.AST]
    , logic :: [Logic.AST]
    , ui    :: [UI.AST]
    } deriving (Generic)

instance ToJSON   BundleCssLogicUi
instance FromJSON BundleCssLogicUi
