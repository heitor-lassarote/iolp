{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.CSS.Codegen where

import Universum

import Language.Codegen
import Language.CSS.AST
import Language.Emit

type CSSCodegen = CodegenT CSSGeneratorState

instance Codegen AST where
    type GeneratorState AST = CSSGeneratorState
    codegen = cssCodegen

data Options = Options
    { indentLevel :: Int
    }

defaultOptions :: Options
defaultOptions = Options 4

data CSSGeneratorState = CSSGeneratorState
    { currentIndentLevel :: Int
    , options :: Options
    }

defaultGeneratorState :: CSSGeneratorState
defaultGeneratorState = CSSGeneratorState 0 defaultOptions

instance HasIndentation CSSGeneratorState where
    getIndentation = indentLevel . options
    getCurrentIndentation = currentIndentLevel
    setCurrentIndentation l st = st { currentIndentLevel = l }

genAttributes
    :: (Emit gen, Monoid gen)
    => [Attribute]
    -> CSSCodegen gen

genAttributes [] = pure mempty
genAttributes ((key, value) : as) = do
    attributes <- genAttributes as
    indent' <- indent
    pure $ mconcat
        [ indent'
        , emit key
        , emit ": "
        , emit value
        , emit ";\n"
        , attributes
        ]

genClasses
    :: (Emit gen, Monoid gen)
    => [Class]
    -> CSSCodegen gen
genClasses [] = pure mempty
genClasses ((Class className attributes) : cs) = do
    classes <- genClasses cs
    attributes' <- withIndent $ genAttributes attributes
    pure $ mconcat
        [ emit className
        , emit " {\n"
        , attributes'
        , emit "}"
        , emit $ if null cs then "" else "\n"
        , classes
        ]

cssCodegen
    :: (Emit gen, Monoid gen)
    => AST
    -> CSSCodegen gen
cssCodegen = \case
    CSS classes -> genClasses classes
