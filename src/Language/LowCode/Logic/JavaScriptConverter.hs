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
                JS.Assign name (convertExpression expression) : convert' next
            L.End Nothing ->
                []
            L.End (Just name) ->
                (JS.Expression $ JS.Call name []) : []
            L.If expression true false next ->
                JS.If (convertExpression expression)
                      (JS.Block $ convert' true)
                      (tryElse false) : convert' next
            L.Print text next ->
                (JS.Expression $ JS.Call "console.log" [convertExpression text]) : convert' next
            L.Start name next ->
                JS.Function (Just name) [] (JS.Block $ convert' next) : [JS.Expression $ JS.Call name []]
            L.Var name expression next ->
                JS.Var name (convertExpression expression) : convert' next
            L.While expression body next ->
                JS.While (convertExpression expression)
                         (JS.Block $ convert' body) : convert' next

        tryElse (L.End Nothing) = Nothing
        tryElse ast             = Just $ JS.Block $ convert' ast

convertExpression :: L.Expression -> JS.Expression
convertExpression = \case
    L.BinaryOp left op right -> JS.BinaryOp (convertExpression left) op (convertExpression right)
    L.Parenthesis expr -> JS.Parenthesis $ convertExpression expr
    L.UnaryOp op expr -> JS.UnaryOp op (convertExpression expr)
    L.Value variable -> JS.Value $ fmap logicTyToJsTy variable

logicTyToJsTy :: L.VariableType -> JS.JSType
logicTyToJsTy (L.BoolTy x) = JS.Boolean x
logicTyToJsTy (L.DoubleTy x) = JS.Number x
logicTyToJsTy (L.IntegerTy x) = JS.Number $ fromInteger x
logicTyToJsTy (L.TextTy x) = JS.Text x
