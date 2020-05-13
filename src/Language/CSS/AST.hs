module Language.CSS.AST where

import Universum

type Attribute = (Text, Text)
type ClassName = Text

data AST
    = CSS [Class]

data Class
    = Class ClassName [Attribute]
