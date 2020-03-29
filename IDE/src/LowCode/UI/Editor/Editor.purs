module LowCode.UI.Editor.Editor where

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

import LowCode.Draggable (Element, newElement)
import LowCode.Draggable as Draggable
import LowCode.UI.Editor.Canvas as Canvas

data Action
    = AddDraggable
    | HandleCanvasMessage (Canvas.Message)

type State =
    { elements :: Map.Map Int Element
    , generator :: Int
    }

_elements :: Lens.Lens' State (Map.Map Int Element)
_elements = Lens.lens _.elements $ _ { elements = _ }

_generator :: Lens.Lens' State Int
_generator = Lens.lens _.generator $ _ { generator = _ }

_canvas :: SProxy "canvas"
_canvas = SProxy

type ChildSlots =
    ( canvas :: Canvas.Slot Int
    )

initialState :: forall i. i -> State
initialState _ =
    { elements: Map.empty
    , generator: 0
    }

component :: forall f i o. H.Component HH.HTML f i o Aff
component =
    H.mkComponent
        { initialState: initialState
        , render: render
        , eval: H.mkEval $ H.defaultEval
            { handleAction = handleAction
            }
        }

render :: State -> H.ComponentHTML Action ChildSlots Aff
render state =
    HH.div_
        [ HH.slot _canvas state.generator mkCanvas unit (Just <<< HandleCanvasMessage)
        , HH.p_ [ HH.text $ "Elements in editor: " <> show (Map.size state.elements) ]
        , HH.button
            [ HE.onClick \_ -> Just AddDraggable
            ]
            [ HH.text "Add draggable thing" ]
        , HH.p_ [ HH.text "I never am really satisfied that I understand anything; because, understand it well as I may, my comprehension can only be an infinitesimal fraction of all I want to understand about the many connections and relations which occur to me, how the matter in question was first thought of or arrived at, etc., etc." ]
        , HH.p_ [ HH.text "In almost every computation a great variety of arrangements for the succession of the processes is possible, and various considerations must influence the selections amongst them for the purposes of a calculating engine. One essential object is to choose that arrangement which shall tend to reduce to a minimum the time necessary for completing the calculation." ]
        , HH.p_ [ HH.text "Many persons who are not conversant with mathematical studies imagine that because the business of [Babbage’s Analytical Engine] is to give its results in numerical notation, the nature of its processes must consequently be arithmetical and numerical, rather than algebraical and analytical. This is an error. The engine can arrange and combine its numerical quantities exactly as if they were letters or any other general symbols; and in fact it might bring out its results in algebraical notation, were provisions made accordingly." ]
        , HH.p_ [ HH.text "The Analytical Engine has no pretensions whatever to originate anything. It can do whatever we know how to order it to perform. It can follow analysis, but it has no power of anticipating any analytical revelations or truths. Its province is to assist us in making available what we are already acquainted with." ]
        ]
  where
    mkCanvas = Canvas.component $
        map (\e -> { component: Draggable.component e, element: e })
            (Array.fromFoldable $ Map.values state.elements)

handleAction
    :: forall o
     . Action
    -> H.HalogenM State Action ChildSlots o Aff Unit
handleAction = case _ of
    AddDraggable -> do
        H.modify_ \st ->
            let id = st.generator
            in
            st { elements = Map.insert id (newElement id) st.elements
               , generator = id + 1
               }

    HandleCanvasMessage msg -> handleCanvasMessage msg

handleCanvasMessage
    :: forall o
     . Canvas.Message
    -> H.HalogenM State Action ChildSlots o Aff Unit
handleCanvasMessage = case _ of
    Canvas.ElementDragged element ->
        H.modify_ \st ->
            Lens.over
                _elements
                (Map.update
                    (Just <<< _ { point = element.point })
                    element.identifier)
                st
