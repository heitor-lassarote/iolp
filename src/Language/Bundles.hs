module Language.Bundles where

import Universum

import Data.Aeson

import qualified Language.CSS.AST as CSS
import qualified Language.LowCode.Logic as Logic
import qualified Language.LowCode.UI as UI

data BundleCssLogicUi = BundleCssLogicUi
    { css   :: Maybe CSS.AST
    , logic :: Maybe Logic.AST
    , ui    :: Maybe UI.AST
    } deriving (Generic)

instance ToJSON   BundleCssLogicUi
instance FromJSON BundleCssLogicUi
