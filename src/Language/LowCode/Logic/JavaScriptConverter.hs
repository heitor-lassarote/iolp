{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.LowCode.Logic.JavaScriptConverter where

import Universum

import qualified Data.Text as Text

import qualified Language.JavaScript.AST as JS
import           Language.LanguageConverter
import qualified Language.LowCode.Logic.AST   as L

instance LanguageConverter L.AST JS.AST where
    convert ast = convert [ast]

-- | For some reason, I couldn't declare this function in Bundle.hs, because GHC
-- would say "no instance for (LanguageConverter [L.AST] JS.AST)".
lToJs :: [L.AST] -> JS.AST
lToJs = convert

instance LanguageConverter [L.AST] JS.AST where
    convert = JS.NonScopedBlock . concatMap convert'
      where
        convert' = \case
            L.Assign _ name expression next ->
                JS.Assign name (convert expression) : convert' next
            L.End ->
                []
            L.Expression _ expression next ->
                (JS.Expression $ convert expression) : convert' next
            L.If _ expression true false next ->
                JS.If (convert expression)
                      (JS.Block $ convert' true)
                      (tryElse false) : convert' next
            L.Return _ expression ->
                [JS.Return (convert <$> expression)]
            L.Start _ name _ arguments next ->
                [JS.Function (Just name) arguments (JS.Block $ convert' next)]
            L.Var _ name _ expression next ->
                JS.Var name (convert expression) : convert' next
            L.While _ expression body next ->
                JS.While (convert expression)
                         (JS.Block $ convert' body) : convert' next

        tryElse L.End = Nothing
        tryElse ast   = Just $ JS.Block $ convert' ast

instance LanguageConverter L.Expression JS.Expression where
    convert = \case
        L.Access expr name -> JS.Access (convert expr) name
        L.BinaryOp left op right -> JS.BinaryOp (convert left) op (convert right)
        L.Call expr exprs -> JS.Call (convert expr) (convert <$> exprs)
        L.Index expr inner -> JS.Index (convert expr) (convert inner)
        L.Parenthesis expr -> JS.Parenthesis $ convert expr
        L.UnaryOp op expr -> JS.UnaryOp op (convert expr)
        L.Value variable -> JS.Value (convert <$> variable)

instance LanguageConverter L.Variable JS.Variable where
    convert (L.Array a) = JS.Array (convert <$> a)
    convert (L.Bool b) = JS.Boolean b
    convert (L.Char c) = JS.Text $! Text.singleton c
    convert (L.Double d) = JS.Number d
    convert (L.Integer i) = JS.Number $! fromInteger i
    convert (L.Record _ fs) = JS.Record (second convert <$> fs)
    convert (L.Text t) = JS.Text t
    convert L.Unit = JS.Void
