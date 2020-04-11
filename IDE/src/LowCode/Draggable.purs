module LowCode.Draggable
   ( Slot
   , Identifier
   , Family
   , Child
   , Draggable
   , Item (..)
   , Action
   , Message (..)
   , Query (..)
   , ChildSlots
   , component
   ) where

import Prelude

import CSS as CSS
import Data.Array as Array
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
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.UIEvent.MouseEvent as ME

import LowCode.MouseEventType (MouseEventType (..), clientXY)
import LowCode.Point (Point)

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

newtype Item m = Item
    { html :: H.Component HH.HTML Query (Array (Item m)) Message m
    }

type State m =
    { element :: Draggable
    , children :: Map.Map Identifier (Item m)
    , generator :: Identifier
    }

_element :: forall m. Lens.Lens' (State m) Draggable
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
    :: forall m
     . Identifier
    -> Point
    -> H.Component HH.HTML Query (Array (Item m)) Message m
component id initialPosition =
    H.mkComponent
        { initialState: \items ->
            { element:
                { point: initialPosition
                , identifier: id
                }
            , children: Map.fromFoldable $ Array.zip (Array.range 0 $ Array.length items) items
            , generator: Array.length items
            }
        , render: render
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
    :: forall m
     . State m
    -> H.ComponentHTML Action ChildSlots m
render state =
    HH.div
        [ HE.onMouseEnter (Just <<< HandleMouseEvent MouseEnter)
        , HE.onMouseDown  (Just <<< HandleMouseEvent MouseDown)
        , HP.class_ $ HH.ClassName "draggable"
        , HCSS.style $ postitionDraggable state.element.point
        ]
        draggableSlots
  where
    mkSlot id (Item item) = HH.slot _inner id item.html [] (Just <<< HandleInner)
    draggableSlots = map (uncurry mkSlot) $ Map.toUnfoldable state.children

handleAction
    :: forall f m
     . Action
    -> H.HalogenM (State m) Action f Message m Unit
handleAction = case _ of
    HandleInner msg -> handleInner msg

    HandleMouseEvent evTy ev -> handleMouseEvent evTy ev

handleInner
    :: forall f i m
     . Message
    -> H.HalogenM (State m) i f Message m Unit
handleInner = case _ of
    Clicked point child mousePos -> H.raise $ Clicked point child mousePos

    Removed id -> H.modify_ \st -> st { children = Map.delete id st.children }

handleMouseEvent
    :: forall f i m
     . MouseEventType
    -> ME.MouseEvent
    -> H.HalogenM (State m) i f Message m Unit
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
    :: forall o i m a
     . Query a
    -> H.HalogenM (State m) i ChildSlots o m (Maybe a)
handleQuery = case _ of
    Dragged family point _ -> case L.uncons family of
        Nothing -> do
            H.modify_ $ Lens.set (_element <<< _point) point
            pure Nothing
        Just child -> do
            void $ H.query _inner child.head $ H.tell $ Dragged child.tail point
            pure Nothing
