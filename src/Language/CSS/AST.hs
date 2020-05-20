module Language.CSS.AST where

import Universum

import Data.Aeson

type Attribute = (Text, Text)
type ClassName = Text

data AST
    = CSS [Class]
    deriving (Eq, Generic, Show)

instance FromJSON AST
instance ToJSON   AST

data Class
    = Class ClassName [Attribute]
    deriving (Eq, Show)

instance FromJSON Class where
    parseJSON = withObject "class" $ \o ->
        Class <$> o .: "class-name"
              <*> o .: "attributes"

instance ToJSON Class where
    toJSON (Class name attributes) = object
        [ "class-name" .= String name
        , "attributes" .= toJSON attributes
        ]
