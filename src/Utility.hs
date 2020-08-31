module Utility where

import Data.Foldable (Foldable, foldr)
import Data.Maybe (Maybe (..))

findMap :: (Foldable t) => (a -> Maybe b) -> t a -> Maybe b
findMap f = foldr go Nothing
  where
    go x Nothing = f x
    go _ found   = found
