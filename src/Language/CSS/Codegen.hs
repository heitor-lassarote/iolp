{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.CSS.Codegen
    ( Options (..)
    , defaultOptions
    , CSSGeneratorState (..)
    , defaultGeneratorState
    , withOptions
    ) where

import Universum

import Data.Aeson (FromJSON, ToJSON)

import Language.Codegen
import Language.CSS.AST
import Language.Emit

type CSSCodegen = CodegenT CSSGeneratorState

instance Codegen AST where
    type GeneratorState AST = CSSGeneratorState

    codegen = cssCodegen

data Options = Options
    { indentLevel :: Int
    } deriving (Eq, Generic, Show, FromJSON, ToJSON)

defaultOptions :: Options
defaultOptions = Options 4

data CSSGeneratorState = CSSGeneratorState
    { currentIndentLevel :: Int
    , options :: Options
    } deriving (Eq, Generic, Show, FromJSON, ToJSON)

defaultGeneratorState :: CSSGeneratorState
defaultGeneratorState = CSSGeneratorState 0 defaultOptions

withOptions :: Options -> CSSGeneratorState
withOptions options' = defaultGeneratorState { options = options' }

instance HasIndentation CSSGeneratorState where
    getIndentation = indentLevel . options
    getCurrentIndentation = currentIndentLevel
    setCurrentIndentation l st = st { currentIndentLevel = l }

genAttributes
    :: (Emit gen, Monoid gen)
    => [Attribute]
    -> CSSCodegen gen
genAttributes = fmap mconcat . traverse genAttribute
  where
    genAttribute (key, value) = mconcat <$> sequence
        [ indent
        , emitM key
        , emitM ": "
        , emitM value
        , emitM ";\n"
        ]

genClasses
    :: (Emit gen, Monoid gen)
    => [Class]
    -> CSSCodegen gen
genClasses = fmap (mconcat . fmap emit . intersperse "\n") . traverse genClass
  where
    genClass (Class className attributes) = mconcat <$> sequence
        [ emitM className
        , emitM " {\n"
        , withIndent $ genAttributes attributes
        , emitM "}"
        ]

cssCodegen
    :: (Emit gen, Monoid gen)
    => AST
    -> CSSCodegen gen
cssCodegen = \case
    CSS classes -> genClasses classes
