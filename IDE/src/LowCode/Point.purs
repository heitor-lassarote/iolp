module LowCode.Point where

import Prelude

import Data.Lens as Lens

type Point =
    { x :: Int
    , y :: Int
    }

newtype Point' = Point' Point

mkPoint :: Int -> Int -> Point
mkPoint x y = { x: x, y: y }

mkPoint' :: Int -> Int -> Point'
mkPoint' x y = Point' $ mkPoint x y

_x :: Lens.Lens' Point Int
_x = Lens.lens _.x $ _ { x = _ }

_y :: Lens.Lens' Point Int
_y = Lens.lens _.y $ _ { y = _ }

_x' :: Lens.Lens' Point' Int
_x' = Lens.lens getter setter
  where
    getter (Point' p) = p.x
    setter (Point' p) x = Point' $ p { x = x }

_y' :: Lens.Lens' Point' Int
_y' = Lens.lens getter setter
  where
    getter (Point' p) = p.y
    setter (Point' p) y = Point' $ p { y = y }

instance semiringPoint' :: Semiring Point' where
    add (Point' a) (Point' b) = Point' { x: a.x + b.x, y: a.y + b.y }
    zero = Point' { x: 0, y: 0 }
    mul (Point' a) (Point' b) = Point' { x: a.x * b.x, y: a.y * b.y }
    one = Point' { x: 1, y: 1 }

instance ringPoint' :: Ring Point' where
    sub (Point' a) (Point' b) = Point' { x: a.x - b.x, y: a.y - b.y }

instance commutativeRingPoint' :: CommutativeRing Point'
