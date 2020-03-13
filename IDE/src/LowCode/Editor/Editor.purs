module LowCode.Editor.Editor where

import Prelude

import Data.Map as M
import Data.Maybe (Maybe (..))
import Data.List as L
import Data.List.Lazy as LL

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

import Jack.Tree as JT

import LowCode.AST
import LowCode.Editor.Icons as Icons

type Node a =
    { graph :: JT.Tree (AST a)
    , id    :: Int
    }

data Query
    = NodeSelection (forall a. Node a)

type State a =
    { ast   :: AST a
    , nodes :: M.Map Int (Node a)
    }

render :: forall m a. State a -> H.ComponentHTML Query () m
render state =
    HH.div
        [ HH.attr (HH.AttrName "class") "icon-div"
        ]
        [ Icons.start
            [ HH.attr (HH.AttrName "draggable") "true"
            , HH.attr (HH.AttrName "class") "start-icon"
            , HE.onDragStart \_ -> Just $ NodeSelection { graph: JT.Node (Start L.Nil) LL.nil, id: 0 }
            ]
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
    initialState = { ast: Start L.Nil, nodes: M.empty }

handleAction :: forall a o m. Query -> H.HalogenM (State a) Query () o m Unit
handleAction = case _ of
    NodeSelection node -> do
       H.modify_ \s -> s { nodes = M.insert node.id node s.nodes }

