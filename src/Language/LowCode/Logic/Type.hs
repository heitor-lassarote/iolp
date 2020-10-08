module Language.LowCode.Logic.Type
    ( Type (..)
    , Field (..)
    , Constructor (..)
    ) where

import Universum hiding (Type)

import Data.Aeson

import Language.Common (Name)
import Utility (withTag)

data Type
    = AlgebraicType !Name
    | ArrayType !Type
    | CharType
    | DoubleType
    | FunctionType ![Type] !Type
    | IntegerType
    | RecordType ![Field Type]
    | TextType
    deriving (Eq, Ord, Show)

instance FromJSON Type where
    parseJSON = withObject "Language.LowCode.Logic.AST.VariableType" \o -> o .: "tag" >>= \case
        "adt"      -> AlgebraicType <$> o .: "name"
        "array"    -> ArrayType <$> o .: "elements"
        "char"     -> pure CharType
        "double"   -> pure DoubleType
        "function" -> FunctionType <$> o .: "arguments" <*> o .: "return"
        "integer"  -> pure IntegerType
        "record"   -> RecordType <$> o .: "fields"
        "text"     -> pure TextType
        other      -> fail $
            "Expected 'array', 'char', 'double', 'function','integer', 'record'\
            \ or 'text', but got '" <> other <> "'."

instance ToJSON Type where
    toJSON = \case
        AlgebraicType name -> withTag "adt"
            [ "name"      .= name
            ]
        ArrayType elements -> withTag "array"
            [ "elements"  .= elements
            ]
        CharType -> withTag "char" []
        DoubleType -> withTag "double" []
        FunctionType arguments ret -> withTag "function"
            [ "arguments" .= arguments
            , "return"    .= ret
            ]
        IntegerType -> withTag "integer" []
        RecordType fields -> withTag "record"
            [ "fields"    .= fields
            ]
        TextType -> withTag "text" []

data Field a = Field
    { fieldName  :: !Name
    , fieldValue :: !a
    } deriving (Eq, Functor, Generic, Ord, Show, FromJSON, ToJSON)

data Constructor a = Constructor
    { constructorAdt   :: !Name
    , constructorName  :: !Name
    , constructorValue :: !(Maybe a)
    } deriving (Eq, Functor, Generic, Ord, Show, FromJSON, ToJSON)
