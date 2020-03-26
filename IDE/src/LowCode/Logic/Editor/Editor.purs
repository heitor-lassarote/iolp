module LowCode.Logic.Editor.Editor where

import Prelude

import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe (..))

import Effect.Aff.Class (class MonadAff)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Web.HTML.Event.DragEvent as DE

import LowCode.Logic.AST as LA
import LowCode.Logic.Editor.Icons as Icons

type Node a =
    { graph :: LA.AST a
    , id    :: Int
    , event :: DE.DragEvent
    }

data Query
    = NodeSelection (forall a. Node a)

type State a =
    { ast   :: LA.AST a
    , nodes :: M.Map Int (Node a)
    }

render :: forall m a. State a -> H.ComponentHTML Query () m
render state =
    HH.div
        [ HP.draggable true
        , HP.class_ $ H.ClassName "icon-div"
        , HE.onDragStart \ev ->
            Just $ NodeSelection { graph: LA.Start L.Nil, id: 0, event: ev }
        ]
        [ Icons.start
            [ HH.attr (HH.AttrName "class") "start-icon"
            ]
        ]

app :: forall q i o m. MonadAff m => H.Component HH.HTML q i o m
app =
    H.mkComponent
        { initialState: const initialState
        , render
        , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
        }
  where
    initialState :: forall a. State a
    initialState = { ast: LA.Start L.Nil, nodes: M.empty }

handleAction :: forall a o m. MonadAff m => Query -> H.HalogenM (State a) Query () o m Unit
handleAction = case _ of
    NodeSelection node -> do
        H.modify_ \s -> s { nodes = M.insert node.id node s.nodes }

