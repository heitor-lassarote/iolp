module LowCode.Draggable where

import Prelude

import CSS as CSS
import Data.Int (toNumber)
import Data.Lens as Lens
import Data.Maybe (Maybe (..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Elements as HEl
import Halogen.HTML.Properties as HP
import Web.UIEvent.MouseEvent as ME

import LowCode.Point (Point)
import LowCode.Util as Util

type Slot = H.Slot Query Message

type Identifier = Int

type Element =
    { point :: Point
    , identifier :: Identifier
    }

newElement :: Identifier -> Element
newElement identifier =
    { point: zero
    , identifier: identifier
    }

_point :: Lens.Lens' Element Point
_point = Lens.lens _.point $ _ { point = _ }

_identifier :: Lens.Lens' Element Identifier
_identifier = Lens.lens _.identifier $ _ { identifier = _ }

class Draggable item where
    identifier :: item -> Identifier
    point :: item -> Point

data Action
    = Drag ME.MouseEvent

data Message
    = Clicked Element Point
    | Remove Identifier

data Query a
    = Dragged Point a

component :: forall i m. Int -> H.Component HH.HTML Query i Message m
component id =
    H.mkComponent
        { initialState: const $ newElement id
        , render: render
        , eval: H.mkEval $ H.defaultEval
            { handleAction = handleAction
            , handleQuery  = handleQuery
            }
        }

postitionElement :: Point -> CSS.CSS
postitionElement point = do
    CSS.left $ CSS.px $ toNumber point.x
    CSS.top  $ CSS.px $ toNumber point.y

render :: forall f m. Element -> H.ComponentHTML Action f m
render element =
    draggableNode_ element.point HH.p
        [ HH.text $ "Drag me around! ID: " <> show element.identifier
        ]

handleAction :: forall f m. Action -> H.HalogenM Element Action f Message m Unit
handleAction = case _ of
    Drag ev -> do
        element <- H.get
        H.raise $ Clicked element $ Util.getClientXY ev

handleQuery :: forall f m a. Query a -> H.HalogenM Element Action f Message m (Maybe a)
handleQuery = case _ of
    Dragged point _ -> do
        H.modify_ _ { point = point }
        pure Nothing

type DragProperty r =
    ( class :: String
    , onMouseDown :: ME.MouseEvent
    , style :: String
    | r
    )

draggableNode
    :: forall r w
     . Point
    -> HEl.Node (DragProperty r) w Action
    -> Array (HP.IProp (DragProperty r) Action)
    -> Array (HH.HTML w Action)
    -> HH.HTML w Action
draggableNode point html props =
    Util.mkNode html $
        [ HP.class_ $ HH.ClassName "draggable"
        , HE.onMouseDown (Just <<< Drag)
        , HCSS.style $ postitionElement point
        ] <> props

draggableNode_
    :: forall r w
     . Point
    -> HEl.Node (DragProperty r) w Action
    -> Array (HH.HTML w Action)
    -> HH.HTML w Action
draggableNode_ point html = draggableNode point html []

draggableLeaf
    :: forall r w
     . Point
    -> HEl.Leaf (DragProperty r) w Action
    -> Array (HP.IProp (DragProperty r) Action)
    -> HH.HTML w Action
draggableLeaf point html props =
    Util.mkLeaf html $
        [ HP.class_ $ HH.ClassName "draggable"
        , HE.onMouseDown (Just <<< Drag)
        , HCSS.style $ postitionElement point
        ] <> props

draggableLeaf_
    :: forall r w
     . Point
    -> HEl.Leaf (DragProperty r) w Action
    -> HH.HTML w Action
draggableLeaf_ point html = draggableLeaf point html []
