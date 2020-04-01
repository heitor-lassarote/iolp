module LowCode.UI.Editor.Editor where

import Prelude

import Data.Maybe (Maybe (..))
import Data.Symbol (SProxy (..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH

import LowCode.UI.Editor.Canvas as Canvas

data Action
    = HandleCanvasMessage (Canvas.Message)

type State = {}

_canvas :: SProxy "canvas"
_canvas = SProxy

type ChildSlots =
    ( canvas :: Canvas.Slot Unit
    )

initialState :: forall i. i -> State
initialState = const {}

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
        [ HH.slot _canvas unit Canvas.component unit (Just <<< HandleCanvasMessage)
        , HH.p_ [ HH.text "I never am really satisfied that I understand anything; because, understand it well as I may, my comprehension can only be an infinitesimal fraction of all I want to understand about the many connections and relations which occur to me, how the matter in question was first thought of or arrived at, etc., etc." ]
        , HH.p_ [ HH.text "In almost every computation a great variety of arrangements for the succession of the processes is possible, and various considerations must influence the selections amongst them for the purposes of a calculating engine. One essential object is to choose that arrangement which shall tend to reduce to a minimum the time necessary for completing the calculation." ]
        , HH.p_ [ HH.text "Many persons who are not conversant with mathematical studies imagine that because the business of [Babbage’s Analytical Engine] is to give its results in numerical notation, the nature of its processes must consequently be arithmetical and numerical, rather than algebraical and analytical. This is an error. The engine can arrange and combine its numerical quantities exactly as if they were letters or any other general symbols; and in fact it might bring out its results in algebraical notation, were provisions made accordingly." ]
        , HH.p_ [ HH.text "The Analytical Engine has no pretensions whatever to originate anything. It can do whatever we know how to order it to perform. It can follow analysis, but it has no power of anticipating any analytical revelations or truths. Its province is to assist us in making available what we are already acquainted with." ]
        ]

handleAction
    :: forall o
     . Action
    -> H.HalogenM State Action ChildSlots o Aff Unit
handleAction = case _ of
    HandleCanvasMessage msg -> handleCanvasMessage msg

handleCanvasMessage
    :: forall o
     . Canvas.Message
    -> H.HalogenM State Action ChildSlots o Aff Unit
handleCanvasMessage = const $ pure unit
