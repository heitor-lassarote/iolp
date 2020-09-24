{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.LowCode.Logic.JavaScriptConverter
    ( module Language.LanguageConverter
    ) where

import Universum

import qualified Data.Text as Text

import qualified Language.JavaScript.AST as JS
import           Language.LanguageConverter
import qualified Language.LowCode.Logic.AST    as L
import qualified Language.LowCode.Logic.Module as L
import           Utility (concatUnzip)

instance LanguageConverter (L.Module L.Type astMetadata) JS.Module where
    convert = JS.Module . fmap convert . L.functions

algebraicToFunction :: L.Name -> [L.Constructor] -> JS.Expression
algebraicToFunction "False" [] = JS.Literal $ JS.Boolean False
algebraicToFunction "True" [] = JS.Literal $ JS.Boolean True
algebraicToFunction "Unit" [] = JS.Literal $ JS.Record []
algebraic name fields
    | null fields = mkRecord
    | otherwise   = JS.Function Nothing names (JS.Expression mkRecord)
  where
    names = map L.constructorName fields
    mkField left right = (left, JS.Literal (JS.Text right))
    mkRecord = JS.Literal (JS.Record (mkField "$" name : zipWith mkField names names))

instance LanguageConverter (L.Function L.Type astMetadata) JS.AST where
    convert = \case
        L.Function _ name _ arguments next ->
            JS.Expression $ JS.Function (Just name) arguments (convert next)

instance LanguageConverter (L.AST L.Type astMetadata) JS.AST where
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
            L.Match _ expression branches next ->
                convertMatch expression branches <> convert' next
            L.Return _ expression ->
                [JS.Return (convert <$> expression)]
            L.Var _ name _ expression next ->
                JS.Var name (convert expression) : convert' next
            L.While _ expression body next ->
                JS.While (convert expression)
                         (JS.Block $ convert' body) : convert' next

        tryElse (L.End _) = Nothing
        tryElse ast       = Just $ JS.Block $ convert' ast

-- TODO: Create a name generator!
convertMatch :: L.Expression L.Type -> [(L.MatchPattern, L.AST L.Type a)] -> [JS.AST]
convertMatch expr []       = [JS.Expression (convert expr)]
convertMatch expr (b : bs) = [JS.Var "_0" (convert expr), mkMatches (b :| bs)]
  where
    mkMatches :: NonEmpty (L.MatchPattern, L.AST L.Type a) -> JS.AST
    mkMatches ((pat, ast) :| []) =
        JS.If (cond exprs) (convert ast) Nothing  -- TODO: throw exception on incomplete pattern matches
      where
        (names, exprs) = mkCondition [] [] (0 :| []) pat
        mkVars = zipWith JS.Var names exprs
    mkMatches ((pat, ast) :| (b : bs)) =
        JS.If (cond exprs) (convert ast) (Just (mkMatches (b :| bs)))
      where
        (names, exprs) = mkCondition [] [] (0 :| []) pat
        mkVars = zipWith JS.Var names exprs

    declareVars vars (JS.Block xs) = JS.Block (vars <> xs)
    declareVars _    _             = error "Panic in convertMatch.declareVars: this is likely a bug. Please report it."

    cond []       = error "Panic in convertMatch.cond: this is likely a bug. Please report it."
    cond (x : xs) = intersperseAnds (x :| xs)

    intersperseAnds (x :| []) = x
    intersperseAnds (x :| (x' : xs)) = JS.BinaryOp x JS.And (intersperseAnds (x' :| xs))

    mkCondition :: [Text] -> [JS.Expression] -> NonEmpty Int -> L.MatchPattern -> ([Text], [JS.Expression])
    mkCondition names exprs tree@(i :| is) = \case
        L.AlgebraicPattern name fields
            | name == "True" && null fields ->
                (names, JS.Literal (JS.Boolean True) : exprs)
            | name == "False" && null fields ->
                (names, JS.UnaryOp JS.Not (JS.Literal (JS.Boolean False)) : exprs)
            | name == "Unit" && null fields ->
                (names, treeName === JS.Literal (JS.Record []) : exprs)
            | otherwise ->
                concatUnzip $ zipWith (mkCondition names (mkDiscriminate name : exprs)) addTree fields
        L.ArrayPattern positions ->
            concatUnzip $ zipWith (mkCondition names (mkLength positions : exprs)) addTree positions
        L.LiteralPattern value -> (names, mkVar i === JS.Literal (convert value) : exprs)
        L.NamePattern name -> (name : names, exprs)
        L.RecordPattern name fields -> undefined
      where
        incTree = i + 1 :| is
        addTree = (:| (i : is)) <$> [0..]
        l === r = JS.BinaryOp l JS.Equal r
        mkName = Text.cons '_' . show
        mkVar = JS.Variable . mkName
        mkLength xs = JS.Access (mkVar i) "length" === JS.Literal (JS.Int $ length xs)
        mkIndexes xs = JS.Index treeName undefined
        mkDiscriminate name = JS.Access treeName "$" === JS.Variable name

        treeName = go tree
          where
            go (i :| [])        = mkVar i
            go (i :| (i' : is)) = JS.Access (go (i' :| is)) (mkName i)

instance LanguageConverter (L.Expression L.Type) JS.Expression where
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
        L.Char c -> JS.Text (Text.singleton c)
        L.Double d -> JS.Number d
        L.Integer i -> JS.Int (fromIntegral i)
        L.Text t -> JS.Text t

instance LanguageConverter (L.Structure L.Type) JS.Expression where
    convert = \case
        L.Algebraic _ name fields -> convertAlgebraic name fields
        L.Array _ a -> JS.Literal $ JS.Array (convert <$> a)
        L.Record _ _ fs -> JS.Literal $ JS.Record (second convert <$> fs)

convertAlgebraic :: L.Name -> [L.Expression L.Type] -> JS.Expression
convertAlgebraic "False" [] = JS.Literal $ JS.Boolean False
convertAlgebraic "True" [] = JS.Literal $ JS.Boolean True
convertAlgebraic "Unit" [] = JS.Literal $ JS.Record []
convertAlgebraic name fields
    | null fields = JS.Variable name
    | otherwise   = JS.Call (JS.Variable name) (convert <$> fields)
