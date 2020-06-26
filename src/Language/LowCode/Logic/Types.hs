-- | This module is meant to be imported qualified.
module Language.LowCode.Logic.Types where

import Universum

import Data.Aeson hiding (Bool)

-- TODO: Add let ... in ... so that we can declare variables and functions?
data Variable
    = Bool Bool
    | Char Char
    | Double Double
    | Integer Integer
    | Text Text
    | Unit
    deriving (Eq, Show)

instance FromJSON Variable where
    parseJSON = withObject "variable" \o -> do
        tag <- o .: "type"
        if tag == "unit"
        then pure Unit
        else do
            value' <- o .: "value"
            case tag of
                "bool"    -> Bool    <$> parseJSON value'
                "char"    -> Char    <$> parseJSON value'
                "double"  -> Double  <$> parseJSON value'
                "integer" -> Integer <$> parseJSON value'
                "text"    -> Text    <$> parseJSON value'
                other     -> fail $
                               "Expected 'bool', 'double', 'integer', 'text' or"
                               <> " 'unit', but got '" <> other <> "'."

instance ToJSON Variable where
    toJSON (Bool b) = object
        [ "type"  .= String "bool"
        , "value" .= b
        ]
    toJSON (Char c) = object
        [ "type"  .= String "char"
        , "value" .= c
        ]
    toJSON (Double d) = object
        [ "type"  .= String "double"
        , "value" .= d
        ]
    toJSON (Integer i) = object
        [ "type"  .= String "integer"
        , "value" .= i
        ]
    toJSON (Text t) = object
        [ "type"  .= String "text"
        , "value" .= t
        ]
    toJSON Unit = object
        [ "type"  .= String "unit"
        ]

data VariableType
    = BoolType
    | CharType
    | DoubleType
    | FunctionType [VariableType] VariableType
    | IntegerType
    | TextType
    | UnitType
    deriving (Eq, Ord, Show)

instance FromJSON VariableType where
    parseJSON = withObject "variable type" \o -> do
        tag <- o .: "type"
        if tag == "function"
        then FunctionType <$> o .: "arguments" <*> o .: "return"
        else case tag of
            "bool"    -> pure BoolType
            "char"    -> pure CharType
            "double"  -> pure DoubleType
            "integer" -> pure IntegerType
            "text"    -> pure TextType
            "unit"    -> pure UnitType
            other     -> fail $
                           "Expected 'bool', 'double', 'function', 'integer', 'text' or"
                           <> " 'unit', but got '" <> other <> "'."

instance ToJSON VariableType where
    toJSON BoolType    = object [ "type" .= String "bool" ]
    toJSON CharType    = object [ "type" .= String "char" ]
    toJSON DoubleType  = object [ "type" .= String "double" ]
    toJSON (FunctionType arguments return') = object
        [ "type"      .= String "function"
        , "arguments" .= arguments
        , "return"    .= return'
        ]
    toJSON IntegerType = object [ "type" .= String "integer" ]
    toJSON TextType    = object [ "type" .= String "text" ]
    toJSON UnitType    = object [ "type" .= String "unit" ]
