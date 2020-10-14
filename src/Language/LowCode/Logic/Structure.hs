module Language.LowCode.Logic.Structure
    ( Structure (..)
    , Field (..)
    , Constructor (..)
    , structure
    , algebraic
    , array
    , record
    ) where

import Universum hiding (try)

import Data.Aeson hiding (Array)
import Text.Megaparsec
import Text.Megaparsec.Char

import Language.Codegen
import Language.Common
import Language.Emit
import Language.LowCode.Logic.Parser
import Utility (withTag)

data Structure a
    = Algebraic !(Constructor a)
    | Array ![a]
    | Record ![Field a]
    deriving (Eq, Functor, Show)

instance (Codegen a) => Codegen (Structure a) where
    type GeneratorState (Structure a) = GeneratorState a

    codegen = \case
        Algebraic constructor -> codegenAlgebraic constructor
        Array a -> emitBetween' "[" "]" $ a `separatedBy'` ", "
        Record fs -> codegenRecord fs
      where
        codegenAlgebraic (Constructor adtName name value') = mconcat <$> sequence
            [ emitM adtName
            , emitM "::"
            , emitM name
            , maybe (emitM "") (emitBetween' "(" ")" . codegen) value'
            ]

        codegenField (Field fieldName expr) = mconcat <$> sequence
            [ emitM fieldName
            , emitM " = "
            , codegen expr
            ]

        codegenRecord fields =
            emitBetween' "{" "}" $ separatedByF codegenField (emit ", ") fields

instance (FromJSON a) => FromJSON (Structure a) where
    parseJSON = withObject "Language.LowCode.Logic.Structure.Structure" \o -> o .: "tag" >>= \case
        "adt"    -> Algebraic <$> o .: "constructor"
        "array"  -> Array     <$> o .: "positions"
        "record" -> Record    <$> o .: "fields"
        other    -> fail $
            "Expected 'adt', 'array' or 'record', but got '" <> other <> "'."

instance (ToJSON a) => ToJSON (Structure a) where
    toJSON = \case
        Algebraic constructor -> withTag "adt" ["constructor" .= constructor]
        Array a -> withTag "array" ["positions" .= a]
        Record fields -> withTag "record" ["fields" .= fields]

data Field a = Field
    { fieldName  :: !Name
    , fieldValue :: !a
    } deriving (Eq, Functor, Generic, Ord, Show, FromJSON, ToJSON)

data Constructor a = Constructor
    { constructorAdt   :: !Name
    , constructorName  :: !Name
    , constructorValue :: !(Maybe a)
    } deriving (Eq, Functor, Generic, Ord, Show, FromJSON, ToJSON)

structure :: Parser a -> Parser (Structure a)
structure p = choice [Algebraic <$> algebraic p, Array <$> array p, Record <$> record p]

algebraic :: Parser a -> Parser (Constructor a)
algebraic p = liftA3 Constructor (try (variableName <* symbol "::")) variableName (optional $ parenthesis p)

array :: Parser a -> Parser [a]
array p = braces (p `sepEndBy` lexeme (char ','))

record :: Parser a -> Parser [Field a]
record p = try $ brackets $ flip sepBy (lexeme (char ',')) do
    fieldName <- variableName
    void (lexeme (char ':'))
    value' <- p
    pure $ Field fieldName value'
