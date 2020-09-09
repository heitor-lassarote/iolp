{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.LowCode.Logic.JavaScriptConverter
    ( module Language.LanguageConverter
    ) where

import Universum

import qualified Data.Text as Text

import qualified Language.JavaScript.AST as JS
import           Language.LanguageConverter
import qualified Language.LowCode.Logic.AST   as L

instance LanguageConverter (L.TopLevel expressionMetadata metadata) JS.AST where
    convert ast = convert [ast]

instance LanguageConverter [L.TopLevel expressionMetadata metadata] JS.AST where
    convert = JS.NonScopedBlock . concatMap convert'
      where
        convert' = \case
            L.Start _ name _ arguments next ->
                [JS.Expression $ JS.Function (Just name) arguments (convert next)]

instance LanguageConverter (L.AST expressionMetadata metadata) JS.AST where
    convert = JS.Block . convert'
      where
        convert' = \case
            L.Assign _ left right next ->
                JS.Assign (convert left) (convert right) : convert' next
            L.End _ ->
                []
            L.Expression _ expression next ->
                (JS.Expression $ convert expression) : convert' next
            L.If _ expression true false next ->
                JS.If (convert expression)
                      (JS.Block $ convert' true)
                      (tryElse false) : convert' next
            L.Return _ expression ->
                [JS.Return (convert <$> expression)]
            L.Var _ name _ expression next ->
                JS.Var name (convert expression) : convert' next
            L.While _ expression body next ->
                JS.While (convert expression)
                         (JS.Block $ convert' body) : convert' next

        tryElse (L.End _) = Nothing
        tryElse ast       = Just $ JS.Block $ convert' ast

instance LanguageConverter (L.Expression metadata) JS.Expression where
    convert = \case
        L.Access _ expr name -> JS.Access (convert expr) name
        L.BinaryOp _ left op right -> JS.BinaryOp (convert left) op (convert right)
        L.Call _ expr exprs -> JS.Call (convert expr) (convert <$> exprs)
        L.Index _ expr inner -> JS.Index (convert expr) (convert inner)
        L.Literal _ literal -> JS.Literal (convert literal)
        L.Parenthesis _ expr -> JS.Parenthesis $ convert expr
        L.Structure _ struct -> convert struct
        L.UnaryOp _ op expr -> JS.UnaryOp op (convert expr)
        L.Variable _ name -> JS.Variable name

instance LanguageConverter L.Literal JS.Literal where
    convert = \case
        L.Char c -> JS.Text $! Text.singleton c
        L.Double d -> JS.Number d
        L.Integer i -> JS.Number $! fromInteger i
        L.Text t -> JS.Text t

instance LanguageConverter (L.Structure metadata) JS.Expression where
    convert = \case
        L.Algebraic _ name fields -> convertAlgebraic name fields
        L.Array _ a -> JS.Literal $ JS.Array (convert <$> a)
        L.Record _ _ fs -> JS.Literal $ JS.Record (second convert <$> fs)

convertAlgebraic :: L.Name -> [L.Expression metadata] -> JS.Expression
convertAlgebraic "False" [] = JS.Literal $ JS.Boolean False
convertAlgebraic "True" [] = JS.Literal $ JS.Boolean True
convertAlgebraic "Unit" [] = JS.Literal $ JS.Record []
convertAlgebraic name fields =
    JS.Function Nothing ["visitor"] (JS.Expression $ JS.Call (JS.Access (JS.Variable "visitor") ("visit" <> name)) (convert <$> fields))
