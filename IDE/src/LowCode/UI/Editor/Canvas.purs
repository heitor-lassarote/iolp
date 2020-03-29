module LowCode.UI.Editor.Canvas where

import Prelude

import Data.Array as Array
import Data.Lens as Lens
import Data.Maybe (Maybe (..))
import Data.Symbol (SProxy (..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Web.Event.Event as WE
import Web.UIEvent.MouseEvent as ME

import LowCode.Draggable (Element)
import LowCode.Draggable as Draggable
import LowCode.Point (Point)

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
    }

_dragStatus :: Lens.Lens' State (Maybe DragStatus)
_dragStatus = Lens.lens _.dragStatus $ _ { dragStatus = _ }

data MouseEventType
    = MouseUp
    | MouseMove

data Action
    = HandleMouseMove MouseEventType ME.MouseEvent
    | HandleInner Draggable.Message

data Message
    = ElementDragged Element

data Query a

_inner :: SProxy "inner"
_inner = SProxy

type ChildSlots =
    ( inner :: Draggable.Slot Int
    )

initialState :: forall i. i -> State
initialState _ =
    { dragStatus: Nothing
    }

type InnerComponent m =
    { component :: H.Component HH.HTML Draggable.Query Unit Draggable.Message m
    , element   :: Element
    }

component
    :: forall i
     . Array (InnerComponent Aff)
    -> H.Component HH.HTML Query i Message Aff
component innerComponents =
    H.mkComponent
        { initialState: initialState
        , render: render innerComponents
        , eval: H.mkEval $ H.defaultEval
            { handleAction = handleAction
            --, handleQuery  = handleQuery
            }
        }

render
    :: forall m
     . Array (InnerComponent m)
    -> State
    -> H.ComponentHTML Action ChildSlots m
render innerComponents state =
    HH.div
        [ HE.onMouseMove (Just <<< HandleMouseMove MouseMove)
        , HE.onMouseUp   (Just <<< HandleMouseMove MouseUp)
        ]
        ((map mkSlot innerComponents)
        <> [ HH.h1_ [ HH.text $ "Elements in Canvas: " <> show (Array.length innerComponents) ] ])
  where
    mkSlot innerComponent =
        HH.slot _inner innerComponent.element.identifier innerComponent.component unit (Just <<< HandleInner)


handleAction :: Action -> H.HalogenM State Action ChildSlots Message Aff Unit
handleAction = case _ of
    HandleMouseMove evTy ev -> case evTy of
        MouseUp -> H.modify_ _ { dragStatus = Nothing }

        MouseMove -> do
            H.liftEffect $ WE.preventDefault $ ME.toEvent ev
            st <- H.get
            case st.dragStatus of
                Nothing -> pure unit
                Just dragStatus -> do
                    let point = Draggable.getClientXY ev + dragStatus.delta
                        id = dragStatus.dragging
                    _ <- H.query _inner id $ H.tell (Draggable.Dragged point)
                    H.raise $ ElementDragged
                        { point: point
                        , identifier: id
                        }

    HandleInner (Draggable.Clicked element mousePos) ->
        H.modify_ _ { dragStatus = Just { dragging: element.identifier
                                        , delta: element.point - mousePos
                                        }
                    }

--handleQuery
--    :: forall f i o a
--     . Query a
--    -> H.HalogenM (State i) Action (ChildSlots f o) Message Aff (Maybe a)
--handleQuery = case _ of
--    StartDrag dragStatus a -> do
--        H.modify_ _ { dragStatus = Just dragStatus }
--        pure $ Just a
