{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.LowCode.Logic.JavaScriptConverter where

import Universum

import qualified Data.Text as Text

import qualified Language.JavaScript.AST as JS
import           Language.LanguageConverter
import qualified Language.LowCode.Logic.AST   as L
import qualified Language.LowCode.Logic.Types as L

instance LanguageConverter L.AST JS.AST where
    convert ast = convert [ast]

instance LanguageConverter [L.AST] JS.AST where
    convert = JS.NonScopedBlock . concatMap convert'
      where
        convert' = \case
            L.Assign name expression next ->
                JS.Assign name (convert expression) : convert' next
            L.End ->
                []
            L.Expression expression next ->
                (JS.Expression $ convert expression) : convert' next
            L.If expression true false next ->
                JS.If (convert expression)
                      (JS.Block $ convert' true)
                      (tryElse false) : convert' next
            L.Return expression ->
                JS.Return (convert <$> expression) : []
            L.Start name _ arguments next ->
                JS.Function (Just name) arguments (JS.Block $ convert' next) : []
            L.Var name _ expression next ->
                JS.Var name (convert expression) : convert' next
            L.While expression body next ->
                JS.While (convert expression)
                         (JS.Block $ convert' body) : convert' next

        tryElse L.End = Nothing
        tryElse ast   = Just $ JS.Block $ convert' ast

instance LanguageConverter L.Expression JS.Expression where
    convert = \case
        L.BinaryOp left op right -> JS.BinaryOp (convert left) op (convert right)
        L.Call expr exprs -> JS.Call (convert expr) (convert <$> exprs)
        L.Parenthesis expr -> JS.Parenthesis $ convert expr
        L.UnaryOp op expr -> JS.UnaryOp op (convert expr)
        L.Value variable -> JS.Value (convert <$> variable)

instance LanguageConverter L.Variable JS.JSType where
    convert (L.Bool    b) = JS.Boolean b
    convert (L.Char    c) = JS.Text $! Text.singleton c
    convert (L.Double  d) = JS.Number d
    convert (L.Integer i) = JS.Number $! fromInteger i
    convert (L.Text    t) = JS.Text t
    convert (L.Unit     ) = JS.Void
