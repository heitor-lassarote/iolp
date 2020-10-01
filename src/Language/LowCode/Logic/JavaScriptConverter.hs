{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.LowCode.Logic.JavaScriptConverter
    ( module Language.LanguageConverter
    ) where

import Universum

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import qualified Language.JavaScript.AST as JS
import           Language.LanguageConverter
import qualified Language.LowCode.Logic.AST    as L
import qualified Language.LowCode.Logic.Module as L
import           Utility (concatUnzip)

instance LanguageConverter (L.Module L.Type astMetadata) JS.Module where
    convert m = JS.Module $ (algebraics <>) $ fmap convert $ L.functions m
      where
        algebraics = Map.foldr go [] $ Map.filterWithKey filterBuiltin $ L.adtTemplates m

        filterBuiltin k _ = k `notElem` ["Bool", "Unit"]

        go constructors asts = map algebraicToFunction constructors <> asts

        algebraicToFunction :: L.Constructor L.Type -> JS.AST
        algebraicToFunction (L.Constructor name fieldMaybe) = JS.Const name case fieldMaybe of
            Nothing -> JS.Literal (JS.Record [mkField "$" name])
            Just _  -> JS.Function Nothing ["value"] (JS.Expression $ mkRecord name)
          where
            mkField left right = (left, JS.Literal (JS.Text right))
            mkRecord n = JS.Literal (JS.Record [mkField "$" n, mkField "value" "value"])

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

-- FIXME: Create a name generator!
convertMatch :: L.Expression L.Type -> [(L.MatchPattern, L.AST L.Type a)] -> [JS.AST]
convertMatch expr []       = [JS.Expression (convert expr)]
convertMatch expr (b : bs) = [JS.Const varName (convert expr), mkMatches (b :| bs)]
  where
    varName = "$temp"

    mkMatches :: NonEmpty (L.MatchPattern, L.AST L.Type a) -> JS.AST
    mkMatches ((pat, ast) :| []) =
        JS.If (cond exprs) (declareVars vars (convert ast)) (Just $ JS.Block [mkThrow])
      where
        (names, exprs) = mkConds pat
        vars = uncurry JS.Const <$> names

        mkThrow = JS.Throw (JS.Call (JS.Variable "Error") [JS.Literal $ JS.Text "Non-exhaustive pattern matches."])
    mkMatches ((pat, ast) :| (b' : bs')) =
        JS.If (cond exprs) (declareVars vars (convert ast)) (Just (mkMatches (b' :| bs')))
      where
        (names, exprs) = mkConds pat
        vars = uncurry JS.Const <$> names

    mkConds = second (reverse . ordNub) . mkCondition [] [] (JS.Variable varName)

    declareVars vars (JS.Block xs) = JS.Block (vars <> xs)
    declareVars _    _             = error "Panic in convertMatch.declareVars: this is likely a bug. Please report it."

    cond []       = error "Panic in convertMatch.cond: this is likely a bug. Please report it."
    cond (x : xs) = intersperseAnds (x :| xs)

    intersperseAnds (x :| []) = x
    intersperseAnds (x :| (x' : xs)) = JS.BinaryOp x JS.And (intersperseAnds (x' :| xs))

    mkCondition
        :: [(JS.Name, JS.Expression)]
        -> [JS.Expression]
        -> JS.Expression
        -> L.MatchPattern
        -> ([(JS.Name, JS.Expression)], [JS.Expression])
    mkCondition names exprs tree = \case
        L.AlgebraicPattern (L.Constructor name Nothing)
            | name == "True" -> (names, JS.Literal (JS.Boolean True) : exprs)
            | name == "False" -> (names, JS.UnaryOp JS.Not (JS.Literal (JS.Boolean False)) : exprs)
            | name == "Unit" -> (names, tree === JS.Literal (JS.Record []) : exprs)
            | otherwise -> (names, mkDiscriminate name : exprs)
        L.AlgebraicPattern (L.Constructor name (Just field)) ->
            mkCondition names (mkDiscriminate name : exprs) (mkAccess "value") field
        L.ArrayPattern positions ->
            concatUnzip $ zipWith (mkCondition names (mkLength positions : mkIndexes <> exprs)) mkIndexes positions
        L.LiteralPattern value -> (names, tree === JS.Literal (convert value) : exprs)
        L.NamePattern name -> ((name, tree) : names, exprs)
        L.RecordPattern fields ->
            let (fieldNames, patterns) = unzip $ map (\(L.Field n v) -> (n, v)) fields
                accesses = map mkAccess fieldNames
             in concatUnzip $ zipWith (mkCondition names exprs) accesses patterns
      where
        l === r = JS.BinaryOp l JS.Equal r
        mkAccess = JS.Access tree
        mkLength xs = mkAccess "length" === JS.Literal (JS.Int $ length xs)
        mkIndexes = [JS.Index tree (JS.Literal (JS.Int i)) | i <- [0..]]
        mkDiscriminate name = mkAccess "$" === JS.Literal (JS.Text name)

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
        L.Algebraic _ constructor -> convertAlgebraic constructor
        L.Array _ a -> JS.Literal $ JS.Array (convert <$> a)
        L.Record _ fs -> JS.Literal $ JS.Record (fmap (\(L.Field n v) -> (n, convert v)) fs)

convertAlgebraic :: L.Constructor (L.Expression L.Type) -> JS.Expression
convertAlgebraic = \case
    L.Constructor name Nothing
        | name == "False" -> JS.Literal $ JS.Boolean False
        | name == "True"  -> JS.Literal $ JS.Boolean True
        | name == "Unit"  -> JS.Literal $ JS.Record []
        | otherwise       -> JS.Variable name
    L.Constructor name (Just field) -> JS.Call (JS.Variable name) [convert field]
