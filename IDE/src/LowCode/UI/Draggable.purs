module LowCode.Draggable
   ( Slot
   , Identifier
   , Family
   , Child
   , Item
   , Action
   , Message (..)
   , Query (..)
   , ChildSlots
   , component
   ) where

import Prelude

import Control.Comonad.Cofree as CF
import CSS as CSS
import Data.Array as Array
import Data.Int (toNumber)
import Data.Lens as Lens
import Data.List.NonEmpty as NEL
import Data.List.Types (List, NonEmptyList)
import Data.Maybe (Maybe (..))
import Data.Symbol (SProxy (..))
import Data.Tree as Tree
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.UIEvent.MouseEvent as ME

import LowCode.MouseEventType (MouseEventType (..), clientXY)
import LowCode.Point (Point)
import LowCode.UI.Element (Tag, toHTML)
import LowCode.UI.Property (Property)

type Slot = H.Slot Query Message

type Identifier = Int
type Family = NonEmptyList Identifier
type Child = List Identifier

type State =
    { item :: Item
    , children :: Tree.Forest Item
    }

_item :: Lens.Lens' State Item
_item = Lens.lens _.item $ _ { item = _ }

-- One thing to consider: the item should store ALL information used to
-- render it, including its position, the tag that created it (Div, Button, P
-- etc), its CSS information (class, background-color, border etc) and children.
type Item =
    { point :: Point
    , identifier :: Identifier
    , tag :: Tag
    , properties :: Array Property
    }

_point :: Lens.Lens' Item Point
_point = Lens.lens _.point $ _ { point = _ }

_identifier :: Lens.Lens' Item Identifier
_identifier = Lens.lens _.identifier $ _ { identifier = _ }

data Action
    = HandleInner Message
    | HandleInput (Tree.Tree Item)
    | HandleMouseEvent MouseEventType ME.MouseEvent

data Message
    = Clicked Point Family Point
    | Entered Family

data Query a

_inner :: SProxy "inner"
_inner = SProxy

type ChildSlots =
    ( inner :: Slot Int
    )

component
    :: forall m
     . H.Component HH.HTML Query (Tree.Tree Item) Message m
component =
    H.mkComponent
        { initialState: \tree ->
            { item: CF.head tree
            , children: CF.tail tree
            }
        , render: \state -> render state  -- η-expansion due to strictness.
        , eval: H.mkEval $ H.defaultEval
            { handleAction = handleAction
            , receive = Just <<< HandleInput
            }
        }

postitionDraggable :: Point -> CSS.CSS
postitionDraggable point = do
    CSS.left $ CSS.px $ toNumber point.x
    CSS.top  $ CSS.px $ toNumber point.y

render
    :: forall m
     . State
    -> H.ComponentHTML Action ChildSlots m
render state = toHTML properties tags state.item.tag
  where
    properties =
        [ HE.onMouseEnter (Just <<< HandleMouseEvent MouseEnter)
        , HE.onMouseDown  (Just <<< HandleMouseEvent MouseDown)
        , HP.class_ $ HH.ClassName "draggable"
        , HCSS.style $ postitionDraggable state.item.point
        ]
    tags = Array.fromFoldable $ map mkSlot state.children

mkSlot
    :: forall m
     . Tree.Tree Item
    -> H.ComponentHTML Action ChildSlots m
mkSlot tree = HH.slot _inner id component tree (Just <<< HandleInner)
  where
    id = (CF.head tree).identifier

handleAction
    :: forall f m
     . Action
    -> H.HalogenM State Action f Message m Unit
handleAction = case _ of
    HandleInner msg -> handleInner msg

    HandleInput tree -> do
        st <- H.get
        when (st.item /= CF.head tree || st.children /= CF.tail tree) $
            H.put $ st { item = CF.head tree, children = CF.tail tree }

    HandleMouseEvent evTy ev -> handleMouseEvent evTy ev

handleInner
    :: forall f i m
     . Message
    -> H.HalogenM State i f Message m Unit
handleInner = case _ of
    Clicked point child mousePos -> do
        st <- H.get
        H.raise $ Clicked point (NEL.cons st.item.identifier child) mousePos

    Entered child -> do
        st <- H.get
        H.raise $ Entered $ NEL.cons st.item.identifier child

handleMouseEvent
    :: forall f i m
     . MouseEventType
    -> ME.MouseEvent
    -> H.HalogenM State i f Message m Unit
handleMouseEvent evTy ev = case evTy of
    MouseDown
        | ME.button ev == 0 -> do
            state <- H.get
            let item = state.item
                family = NEL.singleton item.identifier
            H.raise $ Clicked item.point family $ clientXY ev
        | otherwise -> pure unit

    MouseEnter -> do
        state <- H.get
        H.raise $ Entered $ NEL.singleton state.item.identifier

    _ -> pure unit
