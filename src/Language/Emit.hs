module Language.Emit where

import Data.Text as T

class Emit gen where
    emit :: Text -> gen

instance Emit [Char] where
    emit = T.unpack

instance Emit Text where
    emit = id
