module LowCode.UI.Main where

import Prelude

import Effect (Effect)

import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import LowCode.UI.Editor as Editor

main :: Effect Unit
main = HA.runHalogenAff do
    body <- HA.awaitBody
    runUI Editor.component unit body
