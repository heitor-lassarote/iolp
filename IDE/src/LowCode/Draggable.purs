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

type Slot m = H.Slot (Query m) Message

type Identifier = Int
type Family = NonEmptyList Identifier
type Child = List Identifier

newtype Item m = Item
    { component :: H.Component HH.HTML (Query m) (Array (Item m)) Message m
    }

type State m =
    { element :: Draggable m
    , children :: Map.Map Identifier (Item m)
    , generator :: Identifier
    }

_element :: forall m. Lens.Lens' (State m) (Draggable m)
_element = Lens.lens _.element $ _ { element = _ }

-- One thing to consider: the element should store ALL information used to
-- render it, including its position, the tag that created it (Div, Button, P
-- etc), its CSS information (class, background-color, border etc) and children.
type Draggable m =
    { point :: Point
    , identifier :: Identifier
    , item :: Item m
    }

_point :: forall m. Lens.Lens' (Draggable m) Point
_point = Lens.lens _.point $ _ { point = _ }

_identifier :: forall m. Lens.Lens' (Draggable m) Identifier
_identifier = Lens.lens _.identifier $ _ { identifier = _ }

data Action
    = HandleInner Message
    | HandleMouseEvent MouseEventType ME.MouseEvent

data Message
    = Clicked Point Family Point
    | Entered Family
    | Removed Identifier

data Query m a
    = AddChild Child (Item m) a
    | Drag Child Point a
    | GetItem Child (Item m -> a)

_inner :: SProxy "inner"
_inner = SProxy

type ChildSlots m =
    ( inner :: Slot m Int
    )

component
    :: forall m
     . Item m
    -> Identifier
    -> Point
    -> H.Component HH.HTML (Query m) (Array (Item m)) Message m
component item id initialPosition =
    H.mkComponent
        { initialState: \items ->
            { element:
                { point: initialPosition
                , identifier: id
                , item: item
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
    -> H.ComponentHTML Action (ChildSlots m) m
render state =
    HH.div
        [ HE.onMouseEnter (Just <<< HandleMouseEvent MouseEnter)
        , HE.onMouseDown  (Just <<< HandleMouseEvent MouseDown)
        , HP.class_ $ HH.ClassName "draggable"
        , HCSS.style $ postitionDraggable state.element.point
        ]
        draggableSlots
  where
    mkSlot id (Item item) = HH.slot _inner id item.component [] (Just <<< HandleInner)
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
    Clicked point child mousePos -> do
        st <- H.get
        H.raise $ Clicked point (NEL.cons st.element.identifier child) mousePos

    Entered child -> do
        st <- H.get
        H.raise $ Entered $ NEL.cons st.element.identifier child

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
        state <- H.get
        H.raise $ Entered $ NEL.singleton state.element.identifier

    _ -> pure unit

handleQuery
    :: forall i m a
     . Query m a
    -> H.HalogenM (State m) i (ChildSlots m) Message m (Maybe a)
handleQuery = case _ of
    AddChild family item _ -> case L.uncons family of
        Nothing -> do
            H.modify_ \st ->
                st { children = Map.insert st.generator item st.children
                   , generator = st.generator + 1
                   }
            pure Nothing
        Just child -> do
            void $ H.query _inner child.head $ H.tell $ AddChild child.tail item
            pure Nothing

    Drag family point _ -> case L.uncons family of
        Nothing -> do
            H.modify_ $ Lens.set (_element <<< _point) point
            pure Nothing
        Just child -> do
            void $ H.query _inner child.head $ H.tell $ Drag child.tail point
            when (L.null child.tail) $
                H.modify_ \st -> st { children = Map.delete child.head st.children }
            pure Nothing

    GetItem family f -> case L.uncons family of
        Nothing -> H.get >>= pure <<< Just <<< f <<< _.element.item
        Just child -> H.query _inner child.head (H.request (GetItem child.tail)) >>= pure <<< map f
