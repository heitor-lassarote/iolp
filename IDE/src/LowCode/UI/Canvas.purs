module LowCode.UI.Canvas
    ( Slot
    , Query
    , Message
    , component
    ) where

import Prelude

import Data.Lens as Lens
import Data.List as L
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
import LowCode.Draggable (Family, Identifier, Item (..))
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

type State m =
    { dragStatus :: Maybe DragStatus
    , elements :: Map.Map Identifier (Item m)
    , generator :: Identifier
    , mouseOver :: L.List Identifier
    }

_dragStatus :: forall m. Lens.Lens' (State m) (Maybe DragStatus)
_dragStatus = Lens.lens _.dragStatus $ _ { dragStatus = _ }

_elements :: forall m. Lens.Lens' (State m) (Map.Map Identifier (Item m))
_elements = Lens.lens _.elements $ _ { elements = _ }

_generator :: forall m. Lens.Lens' (State m) Identifier
_generator = Lens.lens _.generator $ _ { generator = _ }

data Action
    = AddDraggable Tag
    | HandleInner Draggable.Message
    | HandleMouseEvent MouseEventType ME.MouseEvent

data Query i

data Message

_inner :: SProxy "inner"
_inner = SProxy

type ChildSlots m =
    ( inner :: Draggable.Slot m Int
    )

initialState :: forall i m. i -> State m
initialState _ =
    { dragStatus: Nothing
    , elements: Map.empty
    , generator: 0
    , mouseOver: L.Nil
    }

component
    :: forall i
     . H.Component HH.HTML Query i Message Aff
component =
    H.mkComponent
        { initialState: initialState
        , render: render
        --, render: HH.memoized eqStates render
        , eval: H.mkEval $ H.defaultEval
            { handleAction = handleAction
            --, handleQuery  = handleQuery
            }
        }
--  where
--    -- Don't render every time the DragStatus is changed, neither when generator
--    -- changes (as it means elements changed):
--    eqStates :: forall m'. State m' -> State m' -> Boolean
--    eqStates a b = a.elements == b.elements

render
    :: forall m
     . State m
    -> H.ComponentHTML Action (ChildSlots m) m
render state =
    HH.div
        [ HE.onMouseEnter (Just <<< HandleMouseEvent MouseEnter)
        , HE.onMouseLeave (Just <<< HandleMouseEvent MouseLeave)
        , HE.onMouseMove  (Just <<< HandleMouseEvent MouseMove)
        , HE.onMouseUp    (Just <<< HandleMouseEvent MouseUp)
        , HP.class_ $ HH.ClassName "canvas"
        ]
        [ HH.h1_
            [ HH.text $ show state.mouseOver
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
    -- FIXME: it feels quite wrong to me that I need to pass item twice...
    mkSlot id item = HH.slot
        _inner
        id
        (Draggable.component item id zero)
        [item]
        (Just <<< HandleInner)

    draggableSlots = map (uncurry mkSlot) $ Map.toUnfoldable state.elements
    createDraggable ev = Just <<< AddDraggable

handleAction
    :: forall o m
     . Action
    -> H.HalogenM (State m) Action (ChildSlots m) o Aff Unit
handleAction = case _ of
    AddDraggable tag ->
        H.modify_ \st ->
            -- FIXME: toHtml creates uses genericComponent, but it doesn't play
            -- well with the current logic as is (because it's not a Draggable).
            let item = Item { component: toHtml tag }
            in
            st { elements  = Map.insert st.generator item st.elements
               , generator = st.generator + 1
               }

    HandleInner msg -> handleInner msg

    HandleMouseEvent evTy ev -> handleMouseEvent evTy ev

handleInner
    :: forall f i o m m'
     . Draggable.Message
    -> H.HalogenM (State m') i f o m Unit
handleInner = case _ of
    Draggable.Clicked point child mousePos ->
        H.modify_ _ { dragStatus = Just { child: child
                                        , delta: point - mousePos
                                        , inBounds: true
                                        }
                    }

    Draggable.Entered child -> H.modify_ _ { mouseOver = NEL.toList child }

    Draggable.Removed id ->
        H.modify_ \st -> st { elements = Map.delete id st.elements }

handleMouseEvent
    :: forall i o m
     . MouseEventType
    -> ME.MouseEvent
    -> H.HalogenM (State m) i (ChildSlots m) o Aff Unit
handleMouseEvent evTy ev = case evTy of
    MouseMove -> do
        let ev' = ME.toEvent ev
        H.liftEffect $ WE.preventDefault ev'
        st <- H.get
        case st.dragStatus of
            Nothing -> pure unit
            Just dragStatus -> do
                let point = clientXY ev + dragStatus.delta
                    child = NEL.uncons dragStatus.child
                --when (point.x >= 0 && point.y >= 0) $
                void $ H.query _inner child.head $ H.tell $ Draggable.Drag child.tail point
                pure unit

    MouseUp -> do
        st <- H.get
        case st.dragStatus of
            Nothing -> pure unit
            Just dragStatus -> case st.mouseOver of
                L.Nil -> pure unit
                -- TODO: think of a way to disable "collision" instead of this
                -- check, as it might be useful for other things as well.
                over@(c L.: cs) -> when (NEL.toList dragStatus.child /= over) $ do
                    let child = NEL.uncons dragStatus.child
                    item' <- H.query _inner child.head $ H.request $ Draggable.GetItem child.tail
                    case item' of
                        Nothing -> pure unit  -- Absurd.
                        Just item -> do
                            void $ H.query _inner c $ H.tell $ Draggable.AddChild cs item
                            H.put $ st { elements = Map.delete child.head st.elements }

        H.modify_ _ { dragStatus = Nothing }

    _ -> pure unit
