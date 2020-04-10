module LowCode.UI.Element where

import Prelude

data Tag
    = Button
    | Div
    | H1
    | P

derive instance eqTag :: Eq Tag
