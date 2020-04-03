module LowCode.Logic.Editor.Icons where

import Prelude

import Halogen.HTML as HH

type Icon = forall p r i. Array (HH.IProp r i) -> HH.HTML p i

ns :: HH.Namespace
ns = HH.Namespace "http://www.w3.org/2000/svg"

start :: Icon
start attrs =
    HH.elementNS
        ns
        ( HH.ElemName "svg")
        ( attrs <> [ HH.attr (HH.AttrName "width")  "16"
                   , HH.attr (HH.AttrName "height") "16"
                   ]
        )
        [ HH.elementNS ns (HH.ElemName "circle")
            [ HH.attr (HH.AttrName "cx") "8"
            , HH.attr (HH.AttrName "cy") "8"
            , HH.attr (HH.AttrName "r")  "8"
            , HH.attr (HH.AttrName "fill") "green"
            , HH.attr (HH.AttrName "stroke") "black"
            , HH.attr (HH.AttrName "stroke-width") "1"
            ]
            []
        , HH.elementNS ns (HH.ElemName "path")
            [ HH.attr (HH.AttrName "d") "M 4 4 L 4 12 L 12 8 Z"
            , HH.attr (HH.AttrName "fill") "black"
            ]
            []
        ]

