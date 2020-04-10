module LowCode.MouseEventType
   ( module ME
   , clientXY
   , pageXY
   , MouseEventType (..)
   ) where

import Web.UIEvent.MouseEvent (MouseEvent, clientX, clientY, pageX, pageY)
import Web.UIEvent.MouseEvent (MouseEvent, fromEvent) as ME

import LowCode.Point (Point)

clientXY :: MouseEvent -> Point
clientXY ev = { x: clientX ev, y: clientY ev }

pageXY :: MouseEvent -> Point
pageXY ev = { x: pageX ev, y: pageY ev }

data MouseEventType
    = Click
    | DoubleClick
    | MouseDown
    | MouseEnter
    | MouseLeave
    | MouseMove
    | MouseOut
    | MouseOver
    | MouseUp
