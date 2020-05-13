module Language.Codegen where

import Universum

import Data.Text as T

import Language.Emit

type CodegenT state = StateT state (ExceptT Text Identity)

class HasIndentation a where
    getIndentation        :: a -> Int
    getCurrentIndentation :: a -> Int
    setCurrentIndentation :: Int -> a -> a
    modifyCurrentIndentation :: (Int -> Int) -> a -> a
    modifyCurrentIndentation f a =
        let i = getCurrentIndentation a
         in setCurrentIndentation (f i) a

evalCodegenT
    :: (Emit gen, Monoid gen)
    => state
    -> CodegenT state gen
    -> Either Text gen
evalCodegenT st = runIdentity . runExceptT . flip evalStateT st

execCodegenT
    :: (Emit gen, Monoid gen)
    => state
    -> CodegenT state gen
    -> Either Text state
execCodegenT st = runIdentity . runExceptT . flip execStateT st

indent :: (Emit gen, HasIndentation state) => CodegenT state gen
indent = do
    indent' <- getCurrentIndentation <$> get
    pure $ emit $ T.replicate indent' " "

withIndent :: (HasIndentation state) => CodegenT state gen -> CodegenT state gen
withIndent action = do
    indent' <- getIndentation <$> get
    modify $ modifyCurrentIndentation (+ indent')
    result <- action
    modify $ modifyCurrentIndentation (subtract indent')
    pure result

class Codegen ast where
    type GeneratorState ast
    codegen :: (Emit gen, Monoid gen) => ast -> CodegenT (GeneratorState ast) gen
