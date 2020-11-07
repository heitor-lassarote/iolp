{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.LowCode.Logic.JavaScriptConverter
    ( module Language.LanguageConverter
    , LogicConverterState (..)
    ) where

import Universum

import           Data.Default.Class
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import qualified Language.JavaScript.AST as JS
import           Language.LanguageConverter
import qualified Language.LowCode.Logic.AST       as L
import qualified Language.LowCode.Logic.Module    as L
import qualified Language.LowCode.Logic.Structure as L
import qualified Language.LowCode.Logic.Type      as L
import           Utility (concatUnzip)

type LogicConverter = State LogicConverterState

newtype LogicConverterState = LogicConverterState
    { lcsAdtCurrentIndex :: Int
    } deriving (Show)

instance Default LogicConverterState where
    def = LogicConverterState (-1)

instance LanguageConverter (L.Module L.Type) JS.Module where
    type ConverterState (L.Module L.Type) JS.Module = LogicConverterState

    convert L.Module {..} = do
        functions' <- traverse convert functions
        pure $ JS.Module (algebraics <> functions') hasMain
      where
        algebraics = Map.foldr go [] $ Map.filterWithKey filterBuiltin adtTemplates

        hasMain = any L.isMainFunction functions

        filterBuiltin k _ = k `notElem` ["Bool", "Unit"]

        go constructors asts = map algebraicToFunction constructors <> asts

        fieldName = "value"

        algebraicToFunction :: L.Constructor L.Type -> JS.AST
        algebraicToFunction (L.Constructor adtName name fieldMaybe) = JS.Const fName case fieldMaybe of
            Nothing -> JS.Literal $ JS.Record [discriminate]
            Just _  -> JS.Function Nothing [fieldName] (JS.Expression record)
          where
            fName = mkAdtName adtName name
            discriminate = ("$", JS.Literal $ JS.Text fName)
            field = (fieldName, JS.Variable fieldName)
            record = JS.Literal $ JS.Record [discriminate, field]

instance LanguageConverter (L.Function L.Type) JS.AST where
    type ConverterState (L.Function L.Type) JS.AST = LogicConverterState

    convert = \case
        L.Function name _ arguments next ->
            JS.Expression . JS.Function (Just name) arguments <$> convert next

instance LanguageConverter [L.AST L.Type] JS.AST where
    type ConverterState [L.AST L.Type] JS.AST = LogicConverterState

    convert = fmap (JS.Block . concat) . traverse convert'
      where
        convert' = \case
            L.Assign left right ->
                sequence [liftA2 JS.Assign (convert left) (convert right)]
            L.Expression expression ->
                sequence [JS.Expression <$> convert expression]
            L.If expression true false ->
                sequence [liftA3 JS.If (convert expression) (convert true) (tryElse false)]
            L.Match expression branches ->
                convertMatch expression branches
            L.Return Nothing ->
                pure [JS.Return $ Just $ JS.Literal $ JS.Record []]
            L.Return (Just expression) ->
                sequence [JS.Return . Just <$> convert expression]
            L.Var name _ expression ->
                sequence [JS.Var name <$> convert expression]
            L.While expression body ->
                sequence [liftA2 JS.While (convert expression) (convert body)]

        tryElse []  = pure Nothing
        tryElse ast = Just <$> convert ast

nextVarName :: LogicConverter JS.Name
nextVarName = do
    modify \s -> s { lcsAdtCurrentIndex = lcsAdtCurrentIndex s + 1 }
    varName

varName :: LogicConverter JS.Name
varName = do
    index <- gets lcsAdtCurrentIndex
    pure $ "$temp" <> show index

-- FIXME: Create a name generator!
convertMatch :: L.Expression L.Type -> [L.Branch L.Type] -> LogicConverter [JS.AST]
convertMatch expr []       = sequence [JS.Expression <$> convert expr]
convertMatch expr (b : bs) = sequence [liftA2 JS.Const nextVarName (convert expr), mkMatches (b :| bs)]
  where
    mkMatches :: NonEmpty (L.Branch L.Type) -> LogicConverter JS.AST
    mkMatches ((L.Branch pat ast) :| []) = finalize
      where
        finalize = do
            (names, exprs) <- mkConds pat
            let vars = uncurry JS.Const <$> names
            case pat of
                L.DiscardPattern _ -> declareVars vars <$> convert ast
                L.NamePattern _ -> declareVars vars <$> convert ast
                _ -> do
                    body <- convert ast
                    pure $ JS.If (cond exprs) (declareVars vars body) (Just $ JS.Block [mkThrow])
        mkThrow = JS.Throw (JS.Call (JS.Variable "Error") [JS.Literal $ JS.Text "Non-exhaustive pattern matches."])
    mkMatches ((L.Branch pat ast) :| (b' : bs')) = do
        (names, exprs) <- mkConds pat
        let vars = uncurry JS.Const <$> names
        body <- convert ast
        matches <- mkMatches (b' :| bs')
        pure $ JS.If (cond exprs) (declareVars vars body) (Just matches)

    mkConds pattern = do
        name <- varName
        conditions <- mkCondition [] [] (JS.Variable name) pattern
        pure $ second ordNub conditions

    declareVars vars (JS.Block xs) = JS.Block (vars <> xs)
    declareVars vars x             = JS.Block (vars <> [x])

    cond []       = JS.Literal $ JS.Boolean True
    cond (x : xs) = intersperseAnds (x :| xs)

    intersperseAnds (x :| []) = x
    intersperseAnds (x :| (x' : xs)) = JS.BinaryOp x JS.And (intersperseAnds (x' :| xs))

    mkCondition
        :: [(JS.Name, JS.Expression)]
        -> [JS.Expression]
        -> JS.Expression
        -> L.MatchPattern
        -> LogicConverter ([(JS.Name, JS.Expression)], [JS.Expression])
    mkCondition names exprs tree = \case
        L.DiscardPattern _name -> pure (names, exprs)
        L.LiteralPattern value -> do
            value' <- convert value
            pure (names, tree === JS.Literal value' : exprs)
        L.NamePattern name -> pure ((name, tree) : names, exprs)
        L.StructurePattern struct -> case struct of
            L.Algebraic (L.Constructor adtName name Nothing)
                | adtName == "Bool" && name == "True" -> pure (names, JS.Literal (JS.Boolean True) : exprs)
                | adtName == "Bool" && name == "False" -> pure (names, JS.UnaryOp JS.Not (JS.Literal (JS.Boolean False)) : exprs)
                | adtName == "Unit" && name == "Unit" -> pure (names, tree === JS.Literal (JS.Record []) : exprs)
                | otherwise -> pure (names, mkDiscriminate adtName name : exprs)
            L.Algebraic (L.Constructor adtName name (Just field)) ->
                mkCondition names (mkDiscriminate adtName name : exprs) (mkAccess "value") field
            L.Array positions ->
                concatUnzip <$> zipWithM (mkCondition names (mkLength positions : mkIndexes <> exprs)) mkIndexes positions
            L.Record fields ->
                let (fieldNames, patterns) = unzip $ map (\(L.Field n v) -> (n, v)) fields
                    accesses = map mkAccess fieldNames
                 in concatUnzip <$> zipWithM (mkCondition names exprs) accesses patterns
      where
        l === r = JS.BinaryOp l JS.Equal r
        mkAccess = JS.Access tree
        mkLength xs = mkAccess "length" === JS.Literal (JS.Int $ length xs)
        mkIndexes = [JS.Index tree (JS.Literal (JS.Int i)) | i <- [0..]]
        mkDiscriminate adtName name = mkAccess "$" === JS.Literal (JS.Text $ mkAdtName adtName name)

instance LanguageConverter (L.Expression L.Type) JS.Expression where
    type ConverterState (L.Expression L.Type) JS.Expression = LogicConverterState

    convert = \case
        L.Access _ expr name -> liftA2 JS.Access (convert expr) (pure name)
        L.BinaryOp _ left op right -> liftA3 JS.BinaryOp (convert left) (pure op) (convert right)
        -- HACK: Along with accessArray in Analyzer, this is a workaound for the
        -- lack of generics.
        L.Call L.TextType (L.Access (L.FunctionType [L.CharType] L.TextType) left "push") [arg] ->
            liftA3 JS.BinaryOp (convert left) (pure JS.Add) (convert arg)
        L.Call _ expr exprs -> liftA2 JS.Call (convert expr) (traverse convert exprs)
        L.Index _ expr inner -> liftA2 JS.Index (convert expr) (convert inner)
        L.Literal _ literal -> JS.Literal <$> convert literal
        L.Parenthesis _ expr -> JS.Parenthesis <$> convert expr
        L.Structure _ struct -> convert struct
        L.UnaryOp _ op expr -> JS.UnaryOp op <$> convert expr
        L.Variable _ name -> pure $ JS.Variable name

instance LanguageConverter L.Literal JS.Literal where
    type ConverterState L.Literal JS.Literal = LogicConverterState

    convert = pure . \case
        L.Char c -> JS.Text $ fromChar c
        L.Double d -> JS.Number d
        L.Integer i -> JS.Int (fromIntegral i)
        L.Text t -> JS.Text $ strip t
      where
        fromChar :: Char -> Text
        fromChar '"' = "\\\""
        fromChar c   = strip c

        strip :: (Show s) => s -> Text
        strip = Text.init . Text.tail . Text.pack . show

instance LanguageConverter (L.Structure (L.Expression L.Type)) JS.Expression where
    type ConverterState (L.Structure (L.Expression L.Type)) JS.Expression = LogicConverterState

    convert = \case
        L.Algebraic constructor -> convertAlgebraic constructor
        L.Array positions -> do
            positions' <- traverse convert positions
            pure $ JS.Literal $ JS.Array positions'
        L.Record fields -> do
            fields' <- traverse (sequence . (\(L.Field name value) -> (name, convert value))) fields
            pure $ JS.Literal $ JS.Record fields'

convertAlgebraic :: L.Constructor (L.Expression L.Type) -> LogicConverter JS.Expression
convertAlgebraic = \case
    L.Constructor adtName name Nothing
        | adtName == "Bool" && name == "False" -> pure $ JS.Literal $ JS.Boolean False
        | adtName == "Bool" && name == "True"  -> pure $ JS.Literal $ JS.Boolean True
        | adtName == "Unit" && name == "Unit"  -> pure $ JS.Literal $ JS.Record []
        | otherwise -> pure $ JS.Variable $ mkAdtName adtName name
    L.Constructor adtName name (Just field) -> do
        field' <- convert field
        pure $ JS.Call (JS.Variable $ mkAdtName adtName name) [field']

mkAdtName :: L.Name -> L.Name -> JS.Name
mkAdtName adtName cName = adtName <> "$" <> cName
