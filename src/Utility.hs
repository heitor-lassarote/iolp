module Utility
    ( biject
    , findMap
    ) where

import Universum hiding (foldr)

import           Data.Foldable (foldr)
import qualified Data.Map.Strict as Map

biject :: (Ord v) => Map k v -> Map v k
biject = Map.fromList . fmap swap . Map.toList

findMap :: (Foldable t) => (a -> Maybe b) -> t a -> Maybe b
findMap f = foldr go Nothing
  where
    go x Nothing = f x
    go _ found   = found
