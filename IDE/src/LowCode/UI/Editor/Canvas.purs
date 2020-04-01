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
import Halogen.HTML.Elements as HEl
import Halogen.HTML.Properties as HP
import Web.Event.Event as WE
import Web.UIEvent.MouseEvent as ME

import LowCode.Draggable (Element, newElement)
import LowCode.Draggable as Draggable
import LowCode.MouseEventType (MouseEventType (..))
import LowCode.Point (Point)
import LowCode.Util as Util
import LowCode.Util.EventTarget (eqEventTarget)

type Slot = H.Slot Query Message

type DragStatus =
    { delta :: Point
    , identifier :: Int
    , onBounds :: Boolean
    }

_delta :: Lens.Lens' DragStatus Point
_delta = Lens.lens _.delta $ _ { delta = _ }

_identifier :: Lens.Lens' DragStatus Int
_identifier = Lens.lens _.identifier $ _ { identifier = _ }

_onBounds :: Lens.Lens' DragStatus Boolean
_onBounds = Lens.lens _.onBounds $ _ { onBounds = _ }

type State =
    { dragStatus :: Maybe DragStatus
    , elements :: Array Int
    , generator :: Int
    }

_dragStatus :: Lens.Lens' State (Maybe DragStatus)
_dragStatus = Lens.lens _.dragStatus $ _ { dragStatus = _ }

_elements :: Lens.Lens' State (Array Int)
_elements = Lens.lens _.elements $ _ { elements = _ }

_generator :: Lens.Lens' State Int
_generator = Lens.lens _.generator $ _ { generator = _ }

data Action w a
    = HandleMouseEvent MouseEventType ME.MouseEvent
    | HandleInner Draggable.Message
    | AddDraggable (HH.HTML w a)

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
    , elements: []
    , generator: 0
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
    :: forall w a m
     . State
    -> H.ComponentHTML (Action w Draggable.Action) ChildSlots m
render state =
    HH.div
        [ HE.onMouseMove  (Just <<< HandleMouseEvent MouseMove)
        , HE.onMouseUp    (Just <<< HandleMouseEvent MouseUp)
        , HE.onMouseEnter (Just <<< HandleMouseEvent MouseEnter)
        , HE.onMouseLeave (Just <<< HandleMouseEvent MouseLeave)
        ] $
        [ HH.h1_
            [ HH.text $ "Elements in Canvas: " <> show (Array.length state.elements) ]
        , HH.div
            [ HP.class_ $ HH.ClassName "sidenav"
            ]
            [ HH.p
                [ HE.onClick \_ -> Just $ AddDraggable $ Draggable.draggableNode_ zero HH.button [] ]
                [ HH.text "Button" ]
            , HH.p
                [ HE.onClick \_ -> Just $ AddDraggable $ Draggable.draggableNode_ zero HH.div [] ]
                [ HH.text "Div" ]
            , HH.p
                [ HE.onClick \_ -> Just $ AddDraggable $ Draggable.draggableNode_ zero HH.h1 [] ]
                [ HH.text "Header" ]
            , HH.p
                [ HE.onClick \_ -> Just $ AddDraggable $ Draggable.draggableNode_ zero HH.p [] ]
                [ HH.text "Paragraph" ]
            ]
        ]
        <> draggableSlots
  where
    mkSlot id =
        HH.slot _inner id (Draggable.component id) unit (Just <<< HandleInner)
    draggableSlots = map mkSlot state.elements

handleAction
    :: forall w a
     . Action w a
    -> H.HalogenM State (Action w a) ChildSlots Message Aff Unit
handleAction = case _ of
    AddDraggable html ->
        H.modify_ \st ->
            st { elements  = Array.snoc st.elements st.generator
               , generator = st.generator + 1
               }

    HandleMouseEvent evTy ev -> handleMouseEvent evTy ev

    HandleInner msg -> handleInner msg

handleInner
    :: forall f i o m
     . Draggable.Message
    -> H.HalogenM State i f o m Unit
handleInner = case _ of
    Draggable.Clicked element mousePos ->
        H.modify_ _ { dragStatus = Just { identifier: element.identifier
                                        , delta: element.point - mousePos
                                        , onBounds: true
                                        }
                    }

    Draggable.Remove id ->
        H.modify_ \st -> st { elements = Array.delete id st.elements }

handleMouseEvent
    :: forall i o
     . MouseEventType
    -> ME.MouseEvent
    -> H.HalogenM State i ChildSlots o Aff Unit
handleMouseEvent evTy ev = case evTy of
    MouseUp -> H.modify_ _ { dragStatus = Nothing }

    MouseMove -> do
        let ev' = ME.toEvent ev
        H.liftEffect $ WE.preventDefault ev'
        st <- H.get
        case st.dragStatus of
            Nothing -> pure unit
            Just dragStatus -> do
                let point = Util.getClientXY ev + dragStatus.delta
                    id = dragStatus.identifier
                    t = isCurrentTarget ev'
                if t && point.x >= 0 && point.y >= 0 then do
                    _ <- H.query _inner id $ H.tell $ Draggable.Dragged point
                    pure unit
                else
                    pure unit

    MouseEnter -> setBounded true =<< H.get

    MouseLeave -> setBounded false =<< H.get
  where
    setBounded :: forall i f o m. Boolean -> State -> H.HalogenM State i f o m Unit
    setBounded b st = case st.dragStatus of
        Nothing -> pure unit
        Just dragStatus ->
            let dragStatus' = dragStatus { onBounds = b }
            in H.put $ st { dragStatus = Just dragStatus' }

    isCurrentTarget :: WE.Event -> Boolean
    isCurrentTarget ev =
        case WE.target ev of
            Nothing -> false
            Just target -> case WE.currentTarget ev of
                Nothing -> false
                Just evTarget -> eqEventTarget target evTarget

handleQuery
    :: forall f i o m a
     . Query a
    -> H.HalogenM State i f o m (Maybe a)
handleQuery _ = pure Nothing
