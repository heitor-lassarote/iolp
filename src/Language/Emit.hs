module Language.Emit where

import Universum

import Data.Text as T

class Emit gen where
    emit :: Text -> gen

instance Emit [Char] where
    emit = T.unpack

instance Emit Text where
    emit = id

emitA :: (Applicative f, Emit gen) => Text -> f gen
emitA = pure . emit
{-# INLINE emitA #-}

emitM :: (Monad m, Emit gen) => Text -> m gen
emitM = emitA
{-# INLINE emitM #-}
