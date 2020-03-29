module LowCode.Util where

import Halogen.HTML as HH
import Halogen.HTML.Elements as HE
import Halogen.HTML.Properties as HP
import Web.UIEvent.MouseEvent as ME

import LowCode.Point (Point)

getClientXY :: ME.MouseEvent -> Point
getClientXY ev = { x: ME.clientX ev, y: ME.clientY ev }

mkNode
    :: forall r w i
     . HE.Node r w i
    -> Array (HP.IProp r i)
    -> Array (HH.HTML w i)
    -> HH.HTML w i
mkNode html props inner = html props inner

mkNode_
    :: forall r w i
     . HE.Node r w i
    -> Array (HH.HTML w i)
    -> HH.HTML w i
mkNode_ html = mkNode html []

mkLeaf
    :: forall r w i
     . HE.Leaf r w i
    -> Array (HP.IProp r i)
    -> HH.HTML w i
mkLeaf html props = html props

mkLeaf_
    :: forall r w i
     . HE.Leaf r w i
    -> HH.HTML w i
mkLeaf_ html = mkLeaf html []
