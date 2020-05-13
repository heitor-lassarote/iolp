module Language.LowCode.Logic.JavaScriptConverter where

import Universum

import qualified Language.JavaScript.AST    as JS
import           Language.LanguageConverter
import qualified Language.LowCode.Logic.AST as L

instance LanguageConverter L.AST JS.AST where
    convert = JS.Function (Just "TODO_addFunctionNames") [] . JS.Block . convert'
      where
        convert' = \case
            L.End -> []
            L.If (left, op, right) true false next ->
                JS.If (logicTyToJsTy <$> left, op, logicTyToJsTy <$> right)
                      (JS.Block $ convert' true)
                      (tryElse false) : convert' next
            L.Print text ast ->
                JS.Call "console.log"
                        [mconcat ["\"", text, "\""]] : convert' ast
            L.Start ast -> convert' ast
            L.Var k v ast -> JS.Var k (logicTyToJsTy v) : convert' ast

        tryElse L.End = Nothing
        tryElse ast   = Just $ JS.Block $ convert' ast

logicTyToJsTy :: L.VariableType -> JS.JSType
logicTyToJsTy (L.FloatTy x) = JS.Number $ realToFrac x
logicTyToJsTy (L.IntegerTy x) = JS.Number $ fromInteger x
logicTyToJsTy (L.TextTy x) = JS.Text x
