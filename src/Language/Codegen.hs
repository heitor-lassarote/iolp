module Language.Codegen where

import Data.Text

import Language.Emit

class Codegen ast where
    codegen :: (Emit gen, Monoid gen) => ast -> Either Text gen
