module Language.Codegen where

import           Universum
import qualified Universum.Unsafe as Unsafe

import qualified Data.Text as T

import Language.Emit

class Codegen ast where
    type GeneratorState ast
    codegen :: (Emit gen, Monoid gen) => ast -> CodegenT (GeneratorState ast) gen

type CodegenT state = StateT state (ExceptT Text Identity)

class HasIndentation a where
    getIndentation        :: a -> Int
    getCurrentIndentation :: a -> Int
    setCurrentIndentation :: Int -> a -> a
    modifyCurrentIndentation :: (Int -> Int) -> a -> a
    modifyCurrentIndentation f a = setCurrentIndentation (f i) a
      where
        i = getCurrentIndentation a

unsafeCodegen
    :: (Codegen ast, Emit gen, Monoid gen)
    => GeneratorState ast
    -> ast
    -> gen
unsafeCodegen st = Unsafe.fromJust . rightToMaybe . evalCodegenT st . codegen

unsafeCodegen'
    :: (GeneratorState ast ~ (), Codegen ast, Emit gen, Monoid gen)
    => ast
    -> gen
unsafeCodegen' = unsafeCodegen ()

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
    indent' <- gets getCurrentIndentation
    emitM $ T.replicate indent' " "

withIndent :: (HasIndentation state) => CodegenT state gen -> CodegenT state gen
withIndent action = do
    indent' <- gets getIndentation
    modify $ modifyCurrentIndentation (+ indent')
    result <- action
    modify $ modifyCurrentIndentation (subtract indent')
    pure result

separatedBy
    :: (Codegen ast, Emit gen, Monoid gen)
    => [ast]
    -> gen
    -> CodegenT (GeneratorState ast) gen
separatedBy xs sep = separatedByF codegen sep xs

separatedBy'
    :: (Codegen ast, Emit gen, Monoid gen)
    => [ast]
    -> Text
    -> CodegenT (GeneratorState ast) gen
separatedBy' xs sep = separatedByF codegen (emit sep) xs

separatedByF
    :: (Applicative f, Monoid gen)
    => (ast -> f gen)
    -> gen
    -> [ast]
    -> f gen
separatedByF f sep = fmap (mconcat . intersperse sep) . traverse f

emitBetween
    :: (Applicative f, Emit gen, Monoid gen)
    => f gen
    -> f gen
    -> f gen
    -> f gen
emitBetween left right middle = mconcat <$> sequenceA [left, middle, right]

emitBetween'
    :: (Applicative f, Emit gen, Monoid gen)
    => Text
    -> Text
    -> f gen
    -> f gen
emitBetween' left right = emitBetween (emitA left) (emitA right)

mconcatA :: (Applicative f, Monoid a) => [f a] -> f a
mconcatA = fmap mconcat . sequenceA
