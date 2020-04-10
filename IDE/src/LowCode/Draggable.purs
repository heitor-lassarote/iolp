module LowCode.Draggable
   ( Slot
   , Identifier
   , Family
   , Child
   , Draggable
   , DragProperty
   , HTMLNode
   , HTMLLeaf
   , Item
   , Action
   , Message (..)
   , Query (..)
   , ChildSlots
   , component
   , draggableNode
   , draggableNode_
   , draggableLeaf
   , draggableLeaf_
   ) where

import Prelude

import CSS as CSS
import Data.Int (toNumber)
import Data.Lens as Lens
import Data.List as L
import Data.List.NonEmpty as NEL
import Data.List.Types (List, NonEmptyList)
import Data.Map as Map
import Data.Maybe (Maybe (..))
import Data.Symbol (SProxy (..))
import Data.Tuple (uncurry)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Elements as HEl
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.UIEvent.MouseEvent as ME

import LowCode.MouseEventType (MouseEventType (..), clientXY)
import LowCode.Point (Point)
import LowCode.Util as Util

type Slot = H.Slot Query Message

type Identifier = Int
type Family = NonEmptyList Identifier
type Child = List Identifier

-- One thing to consider: the element should store ALL information used to
-- render it, including its position, the tag that created it (Div, Button, P
-- etc), its CSS information (class, background-color, border etc) and children.
type Draggable =
    { point :: Point
    , identifier :: Identifier
    }

_point :: Lens.Lens' Draggable Point
_point = Lens.lens _.point $ _ { point = _ }

_identifier :: Lens.Lens' Draggable Identifier
_identifier = Lens.lens _.identifier $ _ { identifier = _ }

type DragProperty r =
    ( class :: String
    , onMouseDown :: ME.MouseEvent
    , style :: String
    | r
    )

type HTMLNode r m = HEl.Node (DragProperty r) (H.ComponentSlot HH.HTML ChildSlots m Action) Action
type HTMLLeaf r m = HEl.Leaf (DragProperty r) (H.ComponentSlot HH.HTML ChildSlots m Action) Action

data HTML r m
    = Node (HTMLNode r m)
    | Leaf (HTMLLeaf r m)

type Item r m =
    { html :: HTMLNode r m
    }

type State r m =
    { element :: Draggable
    , children :: Map.Map Identifier (Item r m)
    , generator :: Identifier
    }

_element :: forall r m. Lens.Lens' (State r m) Draggable
_element = Lens.lens _.element $ _ { element = _ }

data Action
    = HandleInner Message
    | HandleMouseEvent MouseEventType ME.MouseEvent

data Message
    = Clicked Point Family Point
    | Removed Identifier

data Query a
    = Dragged Child Point a

_inner :: SProxy "inner"
_inner = SProxy

type ChildSlots =
    ( inner :: Slot Int
    )

component
    :: forall r i m
     . Identifier
    -> HTMLNode r m
    -> Point
    -> H.Component HH.HTML Query i Message m
component id html initialPosition =
    H.mkComponent
        { initialState: const
            { element:
                { point: initialPosition
                , identifier: id
                }
            , children: Map.empty
            , generator: 0
            }
        , render: render html
        , eval: H.mkEval $ H.defaultEval
            { handleAction = handleAction
            , handleQuery  = handleQuery
            }
        }

postitionDraggable :: Point -> CSS.CSS
postitionDraggable point = do
    CSS.left $ CSS.px $ toNumber point.x
    CSS.top  $ CSS.px $ toNumber point.y

render
    :: forall r m
     . HTMLNode r m
    -> State r m
    -> H.ComponentHTML Action ChildSlots m
render html state = draggableNode state.element.point html
    [ --HE.onMouseEnter (Just <<< HandleMouseEvent MouseEnter)
    ] $
    [ HH.text $ "Drag me around! ID: " <> show state.element.identifier
    ] <> draggableSlots
  where
    mkSlot id item = HH.slot _inner id (component id item.html zero) unit (Just <<< HandleInner)
    draggableSlots = map (uncurry mkSlot) $ Map.toUnfoldable state.children

handleAction
    :: forall f r m
     . Action
    -> H.HalogenM (State r m) Action f Message m Unit
handleAction = case _ of
    HandleInner msg -> handleInner msg

    HandleMouseEvent evTy ev -> handleMouseEvent evTy ev

handleInner
    :: forall f i r m
     . Message
    -> H.HalogenM (State r m) i f Message m Unit
handleInner = case _ of
    Clicked point child mousePos -> H.raise $ Clicked point child mousePos

    Removed id -> H.modify_ \st -> st { children = Map.delete id st.children }

handleMouseEvent
    :: forall f i r m
     . MouseEventType
    -> ME.MouseEvent
    -> H.HalogenM (State r m) i f Message m Unit
handleMouseEvent evTy ev = case evTy of
    MouseDown -> do
        state <- H.get
        let element = state.element
            family = NEL.singleton element.identifier
        H.raise $ Clicked element.point family $ clientXY ev

    MouseEnter -> do
        pure unit

    _ -> pure unit

handleQuery
    :: forall i o r m a
     . Query a
    -> H.HalogenM (State r m) i ChildSlots o m (Maybe a)
handleQuery = case _ of
    Dragged family point _ -> case L.uncons family of
        Nothing -> do
            H.modify_ $ Lens.set (_element <<< _point) point
            pure Nothing
        Just child -> do
            void $ H.query _inner child.head $ H.tell $ Dragged child.tail point
            pure Nothing

dragProperties :: forall r. Point -> Array (HP.IProp (DragProperty r) Action)
dragProperties point =
    [ HP.class_ $ HH.ClassName "draggable"
    , HE.onMouseDown (Just <<< HandleMouseEvent MouseDown)
    , HCSS.style $ postitionDraggable point
    ]

draggableNode
    :: forall r w
     . Point
    -> HEl.Node (DragProperty r) w Action
    -> Array (HP.IProp (DragProperty r) Action)
    -> Array (HH.HTML w Action)
    -> HH.HTML w Action
draggableNode point html props = Util.mkNode html $ dragProperties point <> props

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
    Util.mkLeaf html $ dragProperties point <> props

draggableLeaf_
    :: forall r w
     . Point
    -> HEl.Leaf (DragProperty r) w Action
    -> HH.HTML w Action
draggableLeaf_ point html = draggableLeaf point html []
