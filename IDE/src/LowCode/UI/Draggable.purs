module LowCode.Draggable
   ( Slot
   , Identifier
   , Family
   , Child
   , Item (..)
   , mkItem
   , Draggable
   , Action
   , Message (..)
   , Query (..)
   , ChildSlots
   , component
   , component'
   ) where

import Prelude

import Control.Comonad.Cofree as CF
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
import Data.Tree as Tree
import Data.Tuple (Tuple, uncurry)
import Data.Tuple.Nested (uncurry3, tuple3)
import Data.Unfoldable (fromMaybe)
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

mkItem
    :: forall m
     . H.Component HH.HTML (Query m) (Array (Item m)) Message m
    -> Item m
mkItem c = Item { component: c }

type State m =
    { element :: Draggable
    , children :: Map.Map Identifier (Item m)
    , generator :: Identifier
    }

_element :: forall m. Lens.Lens' (State m) Draggable
_element = Lens.lens _.element $ _ { element = _ }

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

data Action
    = HandleInner Message
    | HandleMouseEvent MouseEventType ME.MouseEvent

data Message
    = Clicked Point Family Point
    | Entered Family
    | Removed Identifier

data Query m a
    = AddChildren Child (Tree.Forest (Item m)) a
    | Drag Child Point a
    | GetAllItems (Tree.Forest (Item m) -> a)
    | GetItems Child (Tree.Forest (Item m) -> a)

_inner :: SProxy "inner"
_inner = SProxy

type ChildSlots m =
    ( inner :: Slot m Int
    )

component
    :: forall m
     . Identifier
    -> Point
    -> H.Component HH.HTML (Query m) (Array (Item m)) Message m
component id initialPosition = component' id initialPosition Nothing

component'
    :: forall m
     . Identifier
    -> Point
    -> Maybe (H.ComponentHTML Action (ChildSlots m) m)
    -> H.Component HH.HTML (Query m) (Array (Item m)) Message m
component' id initialPosition staticHtml =
    H.mkComponent
        { initialState: \items ->
            { element:
                { point: initialPosition
                , identifier: id
                }
            , children: Map.fromFoldable $ Array.zip (Array.range 0 $ Array.length items) items
            , generator: Array.length items
            }
        , render: render staticHtml
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
     . Maybe (H.ComponentHTML Action (ChildSlots m) m)
    -> State m
    -> H.ComponentHTML Action (ChildSlots m) m
render staticHtml state =
    HH.div
        [ HE.onMouseEnter (Just <<< HandleMouseEvent MouseEnter)
        , HE.onMouseDown  (Just <<< HandleMouseEvent MouseDown)
        , HP.class_ $ HH.ClassName "draggable"
        , HCSS.style $ postitionDraggable state.element.point
        ]
        (draggableSlots (Map.toUnfoldable state.children) <> fromMaybe staticHtml)

createChildren :: forall m. Tree.Forest (Item m) -> Array (Item m)
createChildren L.Nil = []
createChildren items =
    let family = L.mapWithIndex (\index item -> tuple3 index (CF.head item) (CF.tail item)) items
        children id child grandchildren =
            mkItem $ component' id zero $ Just $ mkSlot grandchildren id child
        slots = map (uncurry3 children) family
     in Array.fromFoldable slots

mkSlot
    :: forall m
     . Tree.Forest (Item m)
    -> Identifier
    -> Item m
    -> H.ComponentHTML Action (ChildSlots m) m
mkSlot items id (Item item) =
    HH.slot _inner id item.component (createChildren items) (Just <<< HandleInner)

draggableSlots
    :: forall m
     . Array (Tuple Identifier (Item m))
    -> Array (H.ComponentHTML Action (ChildSlots m) m)
draggableSlots = map (uncurry (\id item -> mkSlot L.Nil id item))

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
    AddChildren family items _ -> case L.uncons family of
        Nothing -> do
            H.modify_ \st ->
                let items' = map CF.head items
                    newGenerator = st.generator + L.length items'
                    newMap = Map.fromFoldable $ L.zip (L.range st.generator newGenerator) items'
                in
                st { children = Map.union st.children newMap
                   , generator = newGenerator
                   }
            pure Nothing
        Just child -> do
            st <- H.get
            if Map.isEmpty st.children then do
                -- There are no slots.
                let children = Map.fromFoldable $ Array.zip (Array.range 0 st.generator) $ createChildren items
                    generator = Map.size children
                H.put $ st { children = children
                           , generator = generator
                           }
                pure Nothing
            else do
                void $ H.query _inner child.head $ H.tell $ AddChildren child.tail items
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

    GetAllItems f -> getAllItems f

    GetItems family f -> case L.uncons family of
        Nothing -> getAllItems f
        Just child -> do
            st <- H.get
            case Map.lookup child.head st.children of
                Nothing -> pure Nothing  -- Absurd.
                Just item ->
                    H.query _inner child.head (H.request (GetItems child.tail)) >>=
                        pure <<< map (f <<< L.singleton <<< CF.mkCofree item)
  where
    getAllItems
        :: forall m' a' i' o
         . (Tree.Forest (Item m') -> a')
        -> H.HalogenM (State m') i' (ChildSlots m') o m' (Maybe a')
    getAllItems f = do
        st <- H.get
        items <- H.queryAll _inner $ H.request GetAllItems
        pure $ Just $ f $ L.zipWith CF.mkCofree (Map.values st.children) (Map.values items)
