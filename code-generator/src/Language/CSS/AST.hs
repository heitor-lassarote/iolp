module Language.CSS.AST where

import Universum

import Data.Aeson

type Attribute = (Text, Text)
type ClassName = Text

data AST
    = CSS ![Class]
    deriving (Eq, Generic, Show, FromJSON, ToJSON)

data Class
    = Class !ClassName ![Attribute]
    deriving (Eq, Show)

instance FromJSON Class where
    parseJSON = withObject "Language.LowCode.CSS.AST.AST" \o ->
        Class <$> o .: "className"
              <*> o .: "attributes"

instance ToJSON Class where
    toJSON (Class name attributes) = object
        [ "className"  .= String name
        , "attributes" .= toJSON attributes
        ]
