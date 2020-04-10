module LowCode.UI.Property where

import CSS

type Property =
    { key :: String
    , value :: String
    }

mk :: String -> String -> Property
mk key value = { key, value }

