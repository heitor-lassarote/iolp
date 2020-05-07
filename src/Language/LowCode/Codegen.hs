module Language.LowCode.Codegen where

import Data.Text

import Language.LowCode.Emit

class Codegen ast where
    codegen :: (Emit gen, Monoid gen) => ast -> Either Text gen
