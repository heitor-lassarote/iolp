module LowCode.Draggable where

import Prelude

import CSS as CSS
import Data.Int (toNumber)
import Data.Lens as Lens
import Data.Maybe (Maybe (..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.UIEvent.MouseEvent as ME

import LowCode.Point (Point)

type Slot = H.Slot Query Message

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

class Draggable item where
    identifier :: item -> Int
    point :: item -> Point

data Action
    = Drag ME.MouseEvent

data Message
    = Clicked Element Point

data Query a
    = Dragged Point a

component :: forall i m. Element -> H.Component HH.HTML Query i Message m
component initialElement =
    H.mkComponent
        { initialState: const initialElement
        , render: render
        , eval: H.mkEval $ H.defaultEval
            { handleAction = handleAction
            , handleQuery  = handleQuery
            }
        }

postitionElement :: Point -> CSS.CSS
postitionElement point = do
    CSS.left $ CSS.px $ toNumber point.x
    CSS.top  $ CSS.px $ toNumber point.y

render :: forall f m. Element -> H.ComponentHTML Action f m
render element =
    HH.p
        [ HCSS.style $ postitionElement element.point
        , HP.class_ $ HH.ClassName "draggable"
        , HE.onMouseDown (Just <<< Drag)
        ]
        [ HH.text $ "Drag me around! ID: " <> show element.identifier]

handleAction :: forall f m. Action -> H.HalogenM Element Action f Message m Unit
handleAction = case _ of
    Drag ev -> do
        element <- H.get
        H.raise $ Clicked element $ getClientXY ev

handleQuery :: forall f m a. Query a -> H.HalogenM Element Action f Message m (Maybe a)
handleQuery = case _ of
    Dragged point a -> do
        H.modify_ _ { point = point }
        pure $ Just a

getClientXY :: ME.MouseEvent -> Point
getClientXY ev = { x: ME.clientX ev, y: ME.clientY ev }
