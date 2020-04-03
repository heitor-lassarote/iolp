module LowCode.UI.Editor where

import Prelude

import Data.Maybe (Maybe (..))
import Data.Symbol (SProxy (..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH

import LowCode.UI.Canvas as Canvas

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
