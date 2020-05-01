module LowCode.UI.Element where

import Prelude

import Halogen.HTML as HH
import Unsafe.Coerce (unsafeCoerce)

data Tag
    = Button
    | Div
    | H1
    | P

derive instance eqTag :: Eq Tag

instance showTag :: Show Tag where
    show = case _ of
        Button -> "Button"
        Div    -> "Div"
        H1     -> "H1"
        P      -> "P"

toHTML
    :: forall r p i
     . Array (HH.IProp r i)
    -> Array (HH.HTML p i)
    -> Tag
    -> HH.HTML p i
toHTML props tags = case _ of
    Button -> relaxContraints HH.button props (with "Button")
    Div    -> relaxContraints HH.div    props (with "Div")
    H1     -> relaxContraints HH.h1     props (with "Header")
    P      -> relaxContraints HH.p      props (with "Paragraph")
  where
    with text = [HH.p_ [HH.text text]] <> tags

    relaxContraints :: forall r' p' i' r''. HH.Node r' p' i' -> HH.Node r'' p' i'
    relaxContraints = unsafeCoerce
