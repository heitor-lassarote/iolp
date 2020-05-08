module Language.LowCode.Logic.JavaScriptConverter where

import           Data.Text (Text)
import           GHC.Float (float2Double)

import qualified Language.JavaScript.AST    as JS
import           Language.LanguageConverter
import qualified Language.LowCode.Logic.AST as L

instance LanguageConverter L.AST JS.AST where
    convert = JS.Function "TODO_addFunctionNames" [] . JS.Block . convert'
      where
        convert' = \case
            L.Start ast -> convert' ast
            L.Var k v ast -> JS.Var k (logicTyToJsTy v) : convert' ast
            L.If comp true false next ->
                JS.If (logicTyToJsTy <$> comp) (JS.Block $ convert' true) (tryElse false) : convert' next
            L.Print text ast -> JS.Call "log" [mconcat ["\"", text, "\""]] : convert' ast
            L.End -> []

        tryElse L.End = Nothing
        tryElse ast   = Just $ JS.Block $ convert' ast

logicTyToJsTy (L.FloatTy x) = JS.Number $ float2Double x
logicTyToJsTy (L.IntegerTy x) = JS.Number $ fromInteger x
logicTyToJsTy (L.TextTy x) = JS.Text x
