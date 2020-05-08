module Language.LowCode.UI.HTMLConverter where

import qualified Language.HTML.AST as HTML
import           Language.LanguageConverter
import qualified Language.LowCode.UI.AST as UI

instance LanguageConverter UI.AST HTML.AST where
    convert = id
