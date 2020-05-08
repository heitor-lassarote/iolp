module Language.HTML.Printer
    ( Options (..)
    , defaultOptions
    , GeneratorState (..)
    , defaultGeneratorState
    ) where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import           Data.Either
import           Data.Functor.Identity
import           Data.Text (Text)
import qualified Data.Text as T

import qualified Language.Codegen as C
import           Language.Emit
import           Language.HTML.AST

type HTMLCodegen = StateT GeneratorState (Except Text)

instance C.Codegen AST where
    codegen ast = runIdentity $ runExceptT $ evalStateT (htmlCodegen ast) defaultGeneratorState

data Options = Options
    { compactCode     :: Bool
    , indentLevel     :: Int
    } deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options False 2

data GeneratorState = GeneratorState
    { currentIndentLevel :: Int
    , options            :: Options
    } deriving (Eq, Show)

defaultGeneratorState :: GeneratorState
defaultGeneratorState = GeneratorState 0 defaultOptions

indent :: (Emit gen) => HTMLCodegen gen
indent = do
    indent <- currentIndentLevel <$> get
    pure $ emit $ T.replicate indent " "

withIndent :: HTMLCodegen gen -> HTMLCodegen gen
withIndent action = do
    indent <- indentLevel . options <$> get
    modify $ \st -> st { currentIndentLevel = currentIndentLevel st + indent }
    result <- action
    modify $ \st -> st { currentIndentLevel = currentIndentLevel st - indent }
    pure result

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
        attributes <- genAttributes attributes
        elements <- genElements asts
        indent' <- indent
        pure $ mconcat
            [ indent'
            , emit "<"
            , emit name
            , attributes
            , emit ">\n"
            , elements
            , emit "\n"
            , indent'
            , emit "</"
            , emit name
            , emit ">\n"
            ]
    Text str -> do
        indent' <- indent
        pure $ mconcat [indent', emit str]
