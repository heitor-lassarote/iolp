module LowCode.UI.Editor.Canvas where

import Prelude

import Data.Array as Array
import Data.Lens as Lens
import Data.Map as Map
import Data.Maybe (Maybe (..))
import Data.Symbol (SProxy (..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Web.Event.Event as WE
import Web.UIEvent.MouseEvent as ME

import LowCode.Draggable (Element, newElement)
import LowCode.Draggable as Draggable
import LowCode.Point (Point)
import LowCode.Util as Util

type Slot = H.Slot Query Message

type DragStatus =
    { delta :: Point
    , dragging :: Int
    }

_delta :: Lens.Lens' DragStatus Point
_delta = Lens.lens _.delta $ _ { delta = _ }

_dragging :: Lens.Lens' DragStatus Int
_dragging = Lens.lens _.dragging $ _ { dragging = _ }

type State =
    { dragStatus :: Maybe DragStatus
    , elements :: Map.Map Int Element
    , generator :: Int
    }

_dragStatus :: Lens.Lens' State (Maybe DragStatus)
_dragStatus = Lens.lens _.dragStatus $ _ { dragStatus = _ }

_elements :: Lens.Lens' State (Map.Map Int Element)
_elements = Lens.lens _.elements $ _ { elements = _ }

_generator :: Lens.Lens' State Int
_generator = Lens.lens _.generator $ _ { generator = _ }

data MouseEventType
    = MouseUp
    | MouseMove

data Action
    = HandleMouseMove MouseEventType ME.MouseEvent
    | HandleInner Draggable.Message
    | AddDraggable

data Message

data Query a

_inner :: SProxy "inner"
_inner = SProxy

type ChildSlots =
    ( inner :: Draggable.Slot Int
    )

initialState :: forall i. i -> State
initialState _ =
    { dragStatus: Nothing
    , elements: Map.empty
    , generator: 0
    }

type InnerComponent m =
    { component :: H.Component HH.HTML Draggable.Query Unit Draggable.Message m
    , element   :: Element
    }

component
    :: forall i
     . H.Component HH.HTML Query i Message Aff
component =
    H.mkComponent
        { initialState: initialState
        , render: render
        , eval: H.mkEval $ H.defaultEval
            { handleAction = handleAction
            --, handleQuery  = handleQuery
            }
        }

render
    :: forall m
     . State
    -> H.ComponentHTML Action ChildSlots m
render state =
    HH.div
        [ HE.onMouseMove (Just <<< HandleMouseMove MouseMove)
        , HE.onMouseUp   (Just <<< HandleMouseMove MouseUp)
        ] $
        [ HH.h1_
            [ HH.text $ "Elements in Canvas: " <> show (Map.size state.elements) ]
        , HH.button
            [ HE.onClick \_ -> Just AddDraggable ]
            [ HH.text "Add draggable thing" ]
        ]
        <> draggableSlots
  where
    mkSlot element =
        HH.slot _inner element.identifier (Draggable.component element) unit (Just <<< HandleInner)
    draggableSlots =
        map mkSlot $ Array.fromFoldable $ Map.values state.elements

handleAction :: Action -> H.HalogenM State Action ChildSlots Message Aff Unit
handleAction = case _ of
    AddDraggable -> do
        H.modify_ \st ->
            let id = st.generator
            in
            st { elements = Map.insert id (newElement id) st.elements
               , generator = id + 1
               }

    HandleMouseMove evTy ev -> case evTy of
        MouseUp -> H.modify_ _ { dragStatus = Nothing }

        MouseMove -> do
            H.liftEffect $ WE.preventDefault $ ME.toEvent ev
            st <- H.get
            case st.dragStatus of
                Nothing -> pure unit
                Just dragStatus -> do
                    let point = Util.getClientXY ev + dragStatus.delta
                        id = dragStatus.dragging
                    _ <- H.query _inner id $ H.tell (Draggable.Dragged point)
                    H.put $ Lens.over
                        _elements
                        (Map.update (Just <<< _ { point = point }) id)
                        st

    HandleInner (Draggable.Clicked element mousePos) ->
        H.modify_ _ { dragStatus = Just { dragging: element.identifier
                                        , delta: element.point - mousePos
                                        }
                    }

handleQuery
    :: forall a
     . Query a
    -> H.HalogenM State Action ChildSlots Message Aff (Maybe a)
handleQuery _ = pure Nothing
