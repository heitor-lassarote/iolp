module LowCode.Editor.Main where

import Prelude

import Effect (Effect)

import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import LowCode.Editor.Editor as Editor

main :: Effect Unit
main = HA.runHalogenAff do
    body <- HA.awaitBody
    runUI Editor.app unit body

