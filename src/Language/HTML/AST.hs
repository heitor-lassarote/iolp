module Language.HTML.AST where

import Data.Text

type Attribute = (Text, Text)
type Attributes = [Attribute]
type Name = Text

data AST
    = Tag Name Attributes [AST]
    | Text Text
