module Language.HTML.AST where

import Universum

import Data.Aeson

type Attribute = (Text, Text)
type Attributes = [Attribute]
type Name = Text

data AST
    = Tag Name Attributes [AST]
    | Text Text
    deriving (Eq, Show)

attribute :: Text -> Text -> Attribute
attribute = (,)

tag :: Name -> Attributes -> [AST] -> AST
tag = Tag

text :: Text -> AST
text = Text

instance FromJSON AST where
    parseJSON = withObject "html-ast" \o -> do
        tagMaybe  <- o .:? "tag"
        textMaybe <- o .:? "text"
        case (tagMaybe, textMaybe) of
            (Nothing, Nothing) -> fail "Must provide either a tag xor a text."
            (Nothing, Just t ) -> Text <$> pure t
            (Just t , Nothing) -> Tag  <$> pure t
                                       <*> o .:? "attributes" .!= []
                                       <*> o .:? "ast"        .!= []
            (Just _ , Just _ ) -> fail "Must provide either a tag xor a text."

instance ToJSON AST where
    toJSON (Tag name attributes ast) = object
        [ "tag"        .= String name
        , "attributes" .= toJSON attributes
        , "ast"        .= toJSON ast
        ]
    toJSON (Text t) = object [ "text" .= String t ]
