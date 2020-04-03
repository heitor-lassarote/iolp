module LowCode.UI.Element where

data Tag
    = Button
    | Div
    | H1
    | P

type Property =
    { left :: String
    , right :: String
    }

properties :: Tag -> Array Property
properties = case _ of
    _ -> []
