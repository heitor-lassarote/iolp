module Language.JavaScript.AST
    ( module Language.Common
    , JSType (..)
    , AST (..)
    ) where

import Universum

import Language.Common

data JSType
    = Number Double
    | Text Text
    deriving (Eq, Show)

-- Simplified version to be good enough to create the conversion.
data AST
    = Block [AST]
    | Call Text [Text]
    | Function (Maybe Text) [Text] AST
    | If (Comparison JSType) AST (Maybe AST)
    | Var Text JSType
