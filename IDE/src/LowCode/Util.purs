module LowCode.Util where

import Prelude

import Halogen as H
import Halogen.HTML as HH

genericComponent
    :: forall q i o f m
     . H.ComponentHTML i f m
    -> H.Component HH.HTML q i o m
genericComponent html =
    H.mkComponent
        { initialState: identity
        , render: const html
        , eval: H.mkEval H.defaultEval
        }
