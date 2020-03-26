module Main where

import Prelude

import CSS as CSS
import Data.Array as Array
import Data.Int (toNumber)
import Data.Lens as Lens
import Data.Map as Map
import Data.Maybe (Maybe (..))
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Web.Event.Event as WE
import Web.UIEvent.MouseEvent as ME

import LowCode.Point (Point)

type Element =
    { point :: Point
    , identifier :: Int
    }

newElement :: Int -> Element
newElement identifier =
    { point: zero
    , identifier: identifier
    }

_point :: Lens.Lens' Element Point
_point = Lens.lens _.point $ _ { point = _ }

_identifier :: Lens.Lens' Element Int
_identifier = Lens.lens _.identifier $ _ { identifier = _ }

type DragStatus =
    { delta :: Point
    , dragging :: Int
    }

_delta :: Lens.Lens' DragStatus Point
_delta = Lens.lens _.delta $ _ { delta = _ }

_dragging :: Lens.Lens' DragStatus Int
_dragging = Lens.lens _.dragging $ _ { dragging = _ }

type State =
    { elements :: Map.Map Int Element
    , dragStatus :: Maybe DragStatus
    , generator :: Int
    }

_elements :: Lens.Lens' State (Map.Map Int Element)
_elements = Lens.lens _.elements $ _ { elements = _ }

_dragStatus :: Lens.Lens' State (Maybe DragStatus)
_dragStatus = Lens.lens _.dragStatus $ _ { dragStatus = _ }

_generator :: Lens.Lens' State Int
_generator = Lens.lens _.generator $ _ { generator = _ }

data MouseEventType
    = MouseDown Int
    | MouseUp
    | MouseMove

data Action
    = HandleMouseMove MouseEventType ME.MouseEvent
    | AddDraggable

ui :: forall f i o. H.Component HH.HTML f i o Aff
ui =
    H.mkComponent
        { initialState: const initialState
        , render
        , eval: H.mkEval (H.defaultEval { handleAction = handleAction })
        }
  where
    initialState :: State
    initialState =
        { elements: Map.empty
        , generator: 0
        , dragStatus: Nothing
        }

    elementStyle :: Element -> CSS.CSS
    elementStyle element = do
        CSS.position CSS.absolute
        CSS.left $ CSS.px $ toNumber element.point.x
        CSS.top  $ CSS.px $ toNumber element.point.y
        CSS.width $ CSS.px 200.0
        CSS.backgroundColor $ CSS.rgba 255 255 255 0.66
        CSS.border CSS.solid (CSS.px 2.0) (CSS.rgba 0 0 0 0.5)
        let r = CSS.px 4.0
        CSS.borderRadius r r r r
        let p = CSS.px 8.0
        CSS.padding p p p p

    renderComponents :: forall m. State -> H.ComponentHTML Action () m
    renderComponents state =
        HH.ul_ $
            map (\e -> HH.li
                    [ HCSS.style $ elementStyle e
                    , HE.onMouseDown (Just <<< HandleMouseMove (MouseDown e.identifier))
                    ]
                    [ HH.text "Drag me around!" ])
                (Array.fromFoldable $ Map.values state.elements)

    render :: forall m. State -> H.ComponentHTML Action () m
    render state =
        HH.div
            [ HE.onMouseMove (Just <<< HandleMouseMove MouseMove)
            , HE.onMouseUp (Just <<< HandleMouseMove MouseUp)
            ]
            [ renderComponents state
            , HH.p_ [ HH.text "I never am really satisfied that I understand anything; because, understand it well as I may, my comprehension can only be an infinitesimal fraction of all I want to understand about the many connections and relations which occur to me, how the matter in question was first thought of or arrived at, etc., etc." ]
            , HH.p_ [ HH.text "In almost every computation a great variety of arrangements for the succession of the processes is possible, and various considerations must influence the selections amongst them for the purposes of a calculating engine. One essential object is to choose that arrangement which shall tend to reduce to a minimum the time necessary for completing the calculation." ]
            , HH.p_ [ HH.text "Many persons who are not conversant with mathematical studies imagine that because the business of [Babbage’s Analytical Engine] is to give its results in numerical notation, the nature of its processes must consequently be arithmetical and numerical, rather than algebraical and analytical. This is an error. The engine can arrange and combine its numerical quantities exactly as if they were letters or any other general symbols; and in fact it might bring out its results in algebraical notation, were provisions made accordingly." ]
            , HH.p_ [ HH.text "The Analytical Engine has no pretensions whatever to originate anything. It can do whatever we know how to order it to perform. It can follow analysis, but it has no power of anticipating any analytical revelations or truths. Its province is to assist us in making available what we are already acquainted with." ]
            , HH.button
                [ HE.onClick \_ -> Just AddDraggable
                ]
                [ HH.text "Add draggable thing" ]
            ]

handleAction :: forall o. Action -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
    AddDraggable ->
        H.modify_ \st ->
            let id = st.generator + 1
            in
            st { elements = Map.insert id (newElement id) st.elements
               , generator = id
               }

    HandleMouseMove evTy ev -> case evTy of
        MouseDown id ->
            H.modify_ \st -> case Map.lookup id st.elements of
                Nothing -> st
                Just element ->
                    st { dragStatus = Just { dragging: id
                                           , delta: element.point - getClientXY ev}
                                           }

        MouseUp -> H.modify_ _ { dragStatus = Nothing }

        MouseMove -> do
            H.liftEffect $ WE.preventDefault $ ME.toEvent ev
            H.modify_ \st -> case st.dragStatus of
                Nothing -> st
                Just dragStatus -> Lens.over
                    _elements
                    (Map.update
                        (Just <<< _ { point = getClientXY ev + dragStatus.delta })
                        dragStatus.dragging)
                    st

getClientXY :: ME.MouseEvent -> Point
getClientXY ev = { x: ME.clientX ev, y: ME.clientY ev }

main :: Effect Unit
main = HA.runHalogenAff do
    body <- HA.awaitBody
    runUI ui unit body
