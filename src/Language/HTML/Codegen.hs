{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.HTML.Codegen
    ( Options (..)
    , HTMLGeneratorState (..)
    , withOptions
    ) where

import Universum

import Data.Aeson (FromJSON, ToJSON)
import Data.Default.Class

import Language.Codegen
import Language.Emit
import Language.HTML.AST

type HTMLCodegen = CodegenT HTMLGeneratorState

instance Codegen AST where
    type GeneratorState AST = HTMLGeneratorState
    codegen = htmlCodegen

instance Codegen [AST] where
    type GeneratorState [AST] = HTMLGeneratorState
    codegen = fmap mconcat . traverse htmlCodegen

data Options = Options
    { compactCode     :: Bool
    , indentLevel     :: Int
    } deriving (Eq, Generic, Show, FromJSON, ToJSON)

instance Default Options where
    def = Options False 2

data HTMLGeneratorState = HTMLGeneratorState
    { currentIndentLevel :: Int
    , options            :: Options
    } deriving (Eq, Generic, Show, FromJSON, ToJSON)

instance HasIndentation HTMLGeneratorState where
    getIndentation = indentLevel . options
    getCurrentIndentation = currentIndentLevel
    setCurrentIndentation l st = st { currentIndentLevel = l }

instance Default HTMLGeneratorState where
    def = HTMLGeneratorState 0 def

withOptions :: Options -> HTMLGeneratorState
withOptions options' = def { options = options' }

genAttributes :: (Emit gen, Monoid gen) => Attributes -> HTMLCodegen gen
genAttributes = \case
    (name, value) : xs -> do
        attributes <- genAttributes xs
        pure $ mconcat
            [ emit " "
            , emit name
            , emit "=\""
            , emit value
            , emit "\""
            , attributes
            ]
    [] -> pure $ emit ""

genElements
    :: (Emit gen, Monoid gen)
    => [AST]
    -> HTMLCodegen gen
genElements = \case
    x : xs -> do
        code <- withIndent (htmlCodegen x)
        elements <- genElements xs
        pure $ mconcat [code, elements]
    [] -> pure $ emit ""

htmlCodegen
    :: (Emit gen, Monoid gen)
    => AST
    -> HTMLCodegen gen
htmlCodegen = \case
    Tag name attributes asts -> do
        indent' <- indent
        attributes' <- genAttributes attributes
        elements <- genElements asts
        pure $ mconcat
            [ indent'
            , emit "<"
            , emit name
            , attributes'
            , emit ">\n"
            , elements
            , indent'
            , emit "</"
            , emit name
            , emit ">"
            , emit "\n"
            ]
    Text str -> do
        indent' <- indent
        pure $ mconcat [indent', emit str, emit "\n"]
