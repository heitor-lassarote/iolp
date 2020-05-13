module Language.HTML.Attributes where

import Data.Text

import Language.HTML.AST (Attribute)

title :: Text -> Attribute
title value = ("title", value)
