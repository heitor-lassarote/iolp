{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.LowCode.Logic.JavaScriptConverter where

import Universum

import qualified Language.JavaScript.AST    as JS
import           Language.LanguageConverter
import qualified Language.LowCode.Logic.AST as L

instance LanguageConverter L.AST JS.AST where
    convert = JS.Function (Just "start") [] . JS.Block . convert'
      where
        convert' = \case
            L.Assign name expression next ->
                JS.Assign name (convert expression) : convert' next
            L.End Nothing ->
                []
            L.End (Just name) ->
                (JS.Expression $ JS.Call name []) : []
            L.If expression true false next ->
                JS.If (convert expression)
                      (JS.Block $ convert' true)
                      (tryElse false) : convert' next
            L.Print text next ->
                (JS.Expression $ JS.Call "console.log" [convert text]) : convert' next
            L.Start name next ->
                JS.Function (Just name) [] (JS.Block $ convert' next) : [JS.Expression $ JS.Call name []]
            L.Var name expression next ->
                JS.Var name (convert expression) : convert' next
            L.While expression body next ->
                JS.While (convert expression)
                         (JS.Block $ convert' body) : convert' next

        tryElse (L.End Nothing) = Nothing
        tryElse ast             = Just $ JS.Block $ convert' ast

instance LanguageConverter L.Expression JS.Expression where
    convert = \case
        L.BinaryOp left op right -> JS.BinaryOp (convert left) op (convert right)
        L.Call name exprs -> JS.Call name (convert <$> exprs)
        L.Parenthesis expr -> JS.Parenthesis $ convert expr
        L.UnaryOp op expr -> JS.UnaryOp op (convert expr)
        L.Value variable -> JS.Value (logicTyToJsTy <$> variable)

logicTyToJsTy :: L.VariableType -> JS.JSType
logicTyToJsTy (L.BoolTy x) = JS.Boolean x
logicTyToJsTy (L.DoubleTy x) = JS.Number x
logicTyToJsTy (L.IntegerTy x) = JS.Number $ fromInteger x
logicTyToJsTy (L.TextTy x) = JS.Text x
