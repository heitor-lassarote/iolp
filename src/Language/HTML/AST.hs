module Language.HTML.AST where

import Universum

type Attribute = (Text, Text)
type Attributes = [Attribute]
type Name = Text

data AST
    = Tag Name Attributes [AST]
    | Text Text

attribute :: Text -> Text -> Attribute
attribute = (,)

tag :: Name -> Attributes -> [AST] -> AST
tag = Tag

text :: Text -> AST
text = Text
