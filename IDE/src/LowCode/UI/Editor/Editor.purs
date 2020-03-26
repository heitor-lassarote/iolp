module LowCode.UI.Editor.Editor where

import Prelude

import Data.Map as M
import Data.Maybe (Maybe (..))
import Data.List as L

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

import LowCode.UI.AST

--import LowCode.Editor.UI.Icons as Icons

type Node ast =
    { graph :: AST ast
    , id    :: Int
    }

data Query

type State ast =
    { nodes :: M.Map Int (Node ast)
    }

render :: forall m a. State a -> H.ComponentHTML Query () m
render state =
    HH.div
        []
        [ HH.text "TODO"
        ]

app :: forall q i o m. H.Component HH.HTML q i o m
app =
    H.mkComponent
        { initialState: const initialState
        , render
        , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
        }
  where
    initialState :: forall a. State a
    initialState = { nodes: M.empty }

handleAction :: forall a o m. Query -> H.HalogenM (State a) Query () o m Unit
handleAction = case _ of
    _ -> do
       H.modify_ identity

