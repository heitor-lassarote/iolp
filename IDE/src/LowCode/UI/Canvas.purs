module LowCode.UI.Canvas
    ( Slot
    , Query
    , Message
    , component
    ) where

import Prelude

import Data.Lens as Lens
import Data.List.NonEmpty as NEL
import Data.Map as Map
import Data.Maybe (Maybe (..))
import Data.Symbol (SProxy (..))
import Data.Tuple (uncurry)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event as WE
import Web.UIEvent.MouseEvent as ME

import LowCode.Draggable as Draggable
import LowCode.Draggable (Identifier, Family)
import LowCode.MouseEventType (clientXY, MouseEventType (..))
import LowCode.Point (Point)
import LowCode.UI.Element (Tag (..), toHtml)

type Slot = H.Slot Query Message

type DragStatus =
    { delta :: Point
    , child :: Family
    , inBounds :: Boolean
    }

_delta :: Lens.Lens' DragStatus Point
_delta = Lens.lens _.delta $ _ { delta = _ }

_child :: Lens.Lens' DragStatus Family
_child = Lens.lens _.child $ _ { child = _ }

_inBounds :: Lens.Lens' DragStatus Boolean
_inBounds = Lens.lens _.inBounds $ _ { inBounds = _ }

type State =
    { dragStatus :: Maybe DragStatus
    , elements :: Map.Map Identifier Item
    , generator :: Identifier
    }

_dragStatus :: Lens.Lens' State (Maybe DragStatus)
_dragStatus = Lens.lens _.dragStatus $ _ { dragStatus = _ }

_elements :: Lens.Lens' State (Map.Map Identifier Item)
_elements = Lens.lens _.elements $ _ { elements = _ }

_generator :: Lens.Lens' State Identifier
_generator = Lens.lens _.generator $ _ { generator = _ }

type Item =
    { tag :: Tag
    , point :: Point
    }

data Action
    = AddDraggable Item
    | HandleInner Draggable.Message
    | HandleMouseEvent MouseEventType ME.MouseEvent

data Query i

data Message

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

component
    :: forall i
     . H.Component HH.HTML Query i Message Aff
component =
    H.mkComponent
        { initialState: initialState
        , render: HH.memoized eqStates render
        , eval: H.mkEval $ H.defaultEval
            { handleAction = handleAction
            --, handleQuery  = handleQuery
            }
        }
  where
    -- Don't render every time the DragStatus is changed, neither when generator
    -- changes (as it means elements changed):
    eqStates :: State -> State -> Boolean
    eqStates a b = a.elements == b.elements

render
    :: forall m
     . State
    -> H.ComponentHTML Action ChildSlots m
render state =
    HH.div
        [ HE.onMouseMove  (Just <<< HandleMouseEvent MouseMove)
        , HE.onMouseUp    (Just <<< HandleMouseEvent MouseUp)
        , HE.onMouseEnter (Just <<< HandleMouseEvent MouseEnter)
        , HE.onMouseLeave (Just <<< HandleMouseEvent MouseLeave)
        , HP.class_ $ HH.ClassName "canvas"
        ]
        [ HH.h1_
            [ HH.text $ "Elements in Canvas: " <> show (Map.size state.elements)
            ]
        , HH.div
            [ HP.class_ $ HH.ClassName "canvas-editor"
            ]
            draggableSlots
        , HH.div
            [ HP.classes
                [ HH.ClassName "sidebar"
                , HH.ClassName "canvas-items"
                ]
            ]
            [ HH.p
                [ HE.onClick \ev -> createDraggable ev Button ]
                [ HH.text "Button" ]
            , HH.p
                [ HE.onClick \ev -> createDraggable ev Div ]
                [ HH.text "Div" ]
            , HH.p
                [ HE.onClick \ev -> createDraggable ev H1 ]
                [ HH.text "Header" ]
            , HH.p
                [ HE.onClick \ev -> createDraggable ev P ]
                [ HH.text "Paragraph" ]
            ]
        , HH.div
            [ HP.classes
                [ HH.ClassName "sidebar"
                , HH.ClassName "canvas-properties"
                ]
            ]
            [ HH.h2_ [ HH.text "Properties" ]
            ]
        ]
  where
    mkSlot id item = HH.slot
        _inner
        id
        (Draggable.component id item.point)
        [Draggable.Item { html: toHtml item.tag }]
        (Just <<< HandleInner)
    draggableSlots = map (uncurry mkSlot) $ Map.toUnfoldable state.elements
    createDraggable ev html = Just $ AddDraggable { tag: html, point: zero }

handleAction
    :: forall o
     . Action
    -> H.HalogenM State Action ChildSlots o Aff Unit
handleAction = case _ of
    AddDraggable item ->
        H.modify_ \st ->
            st { elements  = Map.insert st.generator item st.elements
               , generator = st.generator + 1
               }

    HandleInner msg -> handleInner msg

    HandleMouseEvent evTy ev -> handleMouseEvent evTy ev

handleInner
    :: forall f i o m
     . Draggable.Message
    -> H.HalogenM State i f o m Unit
handleInner = case _ of
    Draggable.Clicked point child mousePos ->
        H.modify_ _ { dragStatus = Just { child: child
                                        , delta: point - mousePos
                                        , inBounds: true
                                        }
                    }

    Draggable.Removed id ->
        H.modify_ \st -> st { elements = Map.delete id st.elements }

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
                let point = clientXY ev + dragStatus.delta
                    child = NEL.uncons dragStatus.child
                if point.x >= 0 && point.y >= 0 then
                    void $ H.query _inner child.head $ H.tell $ Draggable.Dragged child.tail point
                else
                    pure unit

    MouseEnter -> setBounded true =<< H.get

    MouseLeave -> setBounded false =<< H.get

    _ -> pure unit
  where
    setBounded
        :: forall i' f o' m
         . Boolean
        -> State
        -> H.HalogenM State i' f o' m Unit
    setBounded b st = case st.dragStatus of
        Nothing -> pure unit
        Just dragStatus ->
            let dragStatus' = dragStatus { inBounds = b }
            in H.put $ st { dragStatus = Just dragStatus' }

--handleQuery
--    :: forall f i o r m a
--     . Query a
--    -> H.HalogenM (State r m) i f o m (Maybe a)
--handleQuery _ = pure Nothing
