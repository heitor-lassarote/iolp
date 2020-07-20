{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.JavaScript.Codegen
    ( JavaScriptCodegen
    , Options (..)
    , JSGeneratorState (..)
    , withOptions
    , javaScriptCodegen
    ) where

import Universum

import           Control.Monad.Trans.Except (throwE)
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Default.Class
import qualified Data.Set  as S

import Language.Codegen
import Language.Emit
import Language.JavaScript.AST

type JavaScriptCodegen = CodegenT JSGeneratorState

instance Codegen AST where
    type GeneratorState AST = JSGeneratorState

    codegen = javaScriptCodegen

instance Codegen Expression where
    type GeneratorState Expression = JSGeneratorState

    codegen = genExpression

data Options = Options
    { bracesOnNewLine :: Bool
    , compactCode     :: Bool
    , indentLevel     :: Int
    , strict          :: Bool
    } deriving (Eq, Generic, Show, FromJSON, ToJSON)

instance Default Options where
    def = Options False False 4 True

data JSGeneratorState = JSGeneratorState
    { currentIndentLevel :: Int
    , options            :: Options
    , symbols            :: Set Text
    } deriving (Eq, Generic, Show, FromJSON, ToJSON)

instance HasIndentation JSGeneratorState where
    getIndentation = indentLevel . options
    getCurrentIndentation = currentIndentLevel
    setCurrentIndentation l st = st { currentIndentLevel = l }

instance Default JSGeneratorState where
    def = JSGeneratorState 0 def S.empty

withOptions :: Options -> JSGeneratorState
withOptions options' = def { options = options' }

indentCompact :: (Emit gen) => JavaScriptCodegen gen
indentCompact = ifM (gets (compactCode . options)) (emitM "") indent

commaSpace :: (Emit gen, Semigroup gen) => JavaScriptCodegen gen
commaSpace = fmap (emit "," <>) space

nl :: (Emit gen) => JavaScriptCodegen gen
nl = ifM (gets (compactCode . options)) (emitM "") (emitM "\n")

space :: (Emit gen) => JavaScriptCodegen gen
space = ifM (gets (compactCode . options)) (emitM "") (emitM " ")

nlIndent :: (Emit gen, Semigroup gen) => JavaScriptCodegen gen
nlIndent = ifM (gets (bracesOnNewLine . options)) (liftA2 (<>) nl indentCompact) space

genConstant
    :: (Emit gen, Monoid gen)
    => Variable
    -> JavaScriptCodegen gen
genConstant = \case
    Array xs -> emitBetween' "[" "]" (separatedBy xs =<< commaSpace)
    Boolean x -> emitM if x then "true" else "false"
    Number x -> emitM $ show x
    Record fs -> do
        cs <- commaSpace
        emitBetween' "{" "}" $ separatedByF (uncurry codegenField) cs fs
    Text x -> emitBetween' "\"" "\"" $ emitM x
    Void -> lift $ throwE "Cannot print variable of type 'Void'."
  where
    codegenField fieldName expr = mconcat <$> sequence
        [ emitM fieldName
        , emitM ":"
        , space
        , codegen expr
        ]

genBlock
    :: (Emit gen, Monoid gen)
    => [AST]
    -> JavaScriptCodegen gen
genBlock asts = mconcat <$> sequence
    [ nlIndent
    , emitM "{"
    , nl
    , separatedByF (withIndent . javaScriptCodegenInternal) mempty asts
    , indentCompact
    , emitM "}"
    , nl
    ]

emitIfValid :: (Emit gen) => Name -> JavaScriptCodegen gen
emitIfValid name
    | isValidName name = emitM name
    | otherwise        = lift $ throwE $ "Invalid identifier name '" <> name <> "'."

genFunction
    :: (Emit gen, Monoid gen)
    => Maybe Name
    -> [Name]
    -> AST
    -> JavaScriptCodegen gen
genFunction name args body = case name of
    Just name' -> mconcat <$> sequence
        [ indentCompact
        , emitM "function"
        , space
        , emitIfValid name'
        , emitM "("
        , pure $ mconcat $ map emit args
        , emitM ")"
        , javaScriptCodegenInternal body
        ]
    Nothing -> mconcat <$> sequence
        [ indentCompact
        , emitM "("
        , pure $ mconcat $ map emit args
        , emitM ")"
        , space
        , emitM "=>"
        , space
        , javaScriptCodegenInternal body
        ]

genAssignmentOrDeclaration
    :: (Emit gen, Monoid gen)
    => Bool
    -> Name
    -> Expression
    -> JavaScriptCodegen gen
genAssignmentOrDeclaration isAssignment name expression = mconcat <$> sequence
    [ indentCompact
    , emitM if isAssignment then "" else "let "
    , emitIfValid name
    , space
    , emitM "="
    , space
    , genExpression expression
    , emitM ";"
    , nl
    ]

undefinedVariableError :: Name -> JavaScriptCodegen gen
undefinedVariableError name =
    lift $ throwE $
        "Undefined symbol '" <> name <> "'. Maybe you forgot to declare it?"

checkVariable :: Name -> JavaScriptCodegen ()
checkVariable name = do
    symbols' <- gets symbols
    if S.member name symbols' then
        pure ()
    else
        undefinedVariableError name

genVariable
    :: (Emit gen, Monoid gen)
    => ValueType Variable
    -> JavaScriptCodegen gen
genVariable (Variable v) = checkVariable v *> emitIfValid v
genVariable (Constant c) = genConstant c

genExpression
    :: (Emit gen, Monoid gen)
    => Expression
    -> JavaScriptCodegen gen
genExpression = \case
    Access expr name -> mconcat <$> sequence
        [ genExpression expr
        , emitM "."
        , emitM name
        ]
    Call expr args -> mconcat <$> sequence
        [ genExpression expr
        , emitBetween' "(" ")" $ args `separatedBy'` ", "
        ]
    Index expr inner -> mconcat <$> sequence
        [ genExpression expr
        , emitBetween' "[" "]" $ genExpression inner
        ]
    BinaryOp left op right -> mconcat <$> sequence
        [ genExpression left
        , emitM $ binarySymbolToText op
        , genExpression right
        ]
    Parenthesis expr -> emitBetween' "(" ")" $ genExpression expr
    UnaryOp op expr -> mconcat <$> sequence
        [ emitM $ unarySymbolToText op
        , genExpression expr
        ]
    Value value -> genVariable value

javaScriptCodegen
    :: (Emit gen, Monoid gen)
    => AST
    -> JavaScriptCodegen gen
javaScriptCodegen ast = do
    strict' <- gets (strict . options)
    let useStrict = emit if strict' then "\"use strict\";\n" else ""
    body <- javaScriptCodegenInternal ast
    pure $ useStrict <> body

javaScriptCodegenInternal
    :: (Emit gen, Monoid gen)
    => AST
    -> JavaScriptCodegen gen
javaScriptCodegenInternal = \case
    Assign name expression -> genAssignmentOrDeclaration True name expression
    Block asts -> genBlock asts
    Expression expression -> mconcat <$> sequence
        [ indentCompact
        , genExpression expression
        , emitM ";"
        , nl
        ]
    Function nameMaybe args inner -> do
        whenJust nameMaybe \name ->
            modify \st -> st { symbols = S.insert name (symbols st) }
        modify \st -> st { symbols = S.union (symbols st) $ S.fromList args }
        genFunction nameMaybe args inner
    If expression t f -> do
        indent' <- indentCompact
        space' <- space
        trueBranch <- go t
        falseBranch <- case f of
            Nothing -> emitM mempty
            Just f' -> do
                falseBranch <- go f'
                pure $ mconcat [indent', emit "else", falseBranch]

        expression' <- genExpression expression
        pure $ mconcat
            [ indent'
            , emit "if"
            , space'
            , emit "("
            , expression'
            , emit ")"
            , trueBranch
            , falseBranch
            ]
    NonScopedBlock asts -> mconcat <$> traverse javaScriptCodegenInternal asts
    Return Nothing -> mconcat <$> sequence
        [ indentCompact
        , emitM "return;"
        , nl
        ]
    Return (Just expression) -> mconcat <$> sequence
        [ indentCompact
        , emitM "return "
        , genExpression expression
        , emitM ";"
        , nl
        ]
    Var name expression -> do
        modify \st -> st { symbols = S.insert name (symbols st) }
        genAssignmentOrDeclaration False name expression
    While expression body -> mconcat <$> sequence
        [ indentCompact
        , emitM "while"
        , space
        , emitBetween' "(" ")" $ genExpression expression
        , go body
        ]
  where
    go = javaScriptCodegenInternal
