module LowCode.UI.Element where

import Prelude

import Halogen as H
import Halogen.HTML as HH

import LowCode.Util (genericComponent)

data Tag
    = Button
    | Div
    | H1
    | P

derive instance eqTag :: Eq Tag

toHtml :: forall q i o m. Tag -> H.Component HH.HTML q i o m
toHtml tag = genericComponent $ case tag of
    Button -> HH.button_ [ HH.text "Press me" ]
    Div    -> HH.div_ [ HH.text "Spanish inquisition" ]
    H1     -> HH.h1_ [ HH.text "Header" ]
    P      -> HH.p_ [ HH.text "Paragraph" ]
