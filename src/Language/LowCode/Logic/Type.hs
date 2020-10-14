module Language.LowCode.Logic.Type
    ( Type (..)
    , Field (..)
    , Constructor (..)
    , typeName
    ) where

import Universum hiding (Type, try)

import Data.Aeson
import Text.Megaparsec

import Language.Common (Name)
import Language.LowCode.Logic.Parser
import Language.LowCode.Logic.Structure
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
    parseJSON = withObject "Language.LowCode.Logic.Type.Type" \o -> o .: "tag" >>= \case
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

typeName :: Parser Type
typeName = try tupleFunc <|> funcOrType
  where
    types = choice
        [ TextType      <$  symbol "Text"
        , RecordType    <$> record typeName
        , IntegerType   <$  symbol "Integer"
        , DoubleType    <$  symbol "Double"
        , CharType      <$  symbol "Char"
        , ArrayType     <$> braces typeName
        , AlgebraicType <$> variableName
        ]

    right = symbol "->" *> typeName

    funcOrType = do
        left <- parenthesis typeName <|> types
        pure . maybe left (FunctionType [left]) =<< optional right

    tupleFunc = liftA2 FunctionType (tuple typeName) right
