module LowCode.Util.EventTarget (eqEventTarget) where

import Web.Event.EventTarget (EventTarget)

foreign import eqEventTarget :: EventTarget -> EventTarget -> Boolean

