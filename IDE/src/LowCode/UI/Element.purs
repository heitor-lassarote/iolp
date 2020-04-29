module LowCode.UI.Element where

import Prelude

import Halogen as H
import Halogen.HTML as HH

data Tag
    = Button
    | Div
    | H1
    | P

derive instance eqTag :: Eq Tag

toComponent
    :: forall q i i' f o m
     . Tag
    -> (H.ComponentHTML i' f m -> H.Component HH.HTML q i o m)
    -> H.Component HH.HTML q i o m
toComponent tag mkComponent = mkComponent $ case tag of
    Button -> HH.button_ [ HH.text "Press me" ]
    Div    -> HH.div_ [ HH.text "Spanish inquisition" ]
    H1     -> HH.h1_ [ HH.text "Header" ]
    P      -> HH.p_ [ HH.text "Paragraph" ]
