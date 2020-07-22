module Language.HTML.AST where

import Universum

import Data.Aeson

import Language.Common (Name)

type Attribute = (Text, Text)
type Attributes = [Attribute]

data AST
    = Tag Name Attributes [AST]
    | Text Text
    deriving (Eq, Show)

instance FromJSON AST where
    parseJSON = withObject "Language.HTML.AST.AST" \o -> do
        tagMaybe  <- o .:? "tag"
        textMaybe <- o .:? "text"
        case (tagMaybe, textMaybe) of
            (Nothing, Just t ) -> Text <$> pure t
            (Just t , Nothing) -> Tag  <$> pure t
                                       <*> o .:? "attributes" .!= []
                                       <*> o .:? "ast"        .!= []
            _                  -> fail "Must provide either a tag xor a text."

instance ToJSON AST where
    toJSON (Tag name attributes ast) = object
        [ "tag"        .= String name
        , "attributes" .= toJSON attributes
        , "ast"        .= toJSON ast
        ]
    toJSON (Text t) = object [ "text" .= String t ]

link :: [Name] -> [Name] -> [AST] -> AST
link cssFiles jsFiles html =
    Tag "html" []
        [ Tag "head" [] (includeStylesheets cssFiles <> includeScripts jsFiles)
        , Tag "body" [] html
        ]
  where
    includeStylesheets fs = [Tag "link" [("rel", "stylesheet"), ("type", "text/css"), ("href", f)] [] | f <- fs]
    includeScripts fs = [Tag "script" [("src", f), ("type", "text/javascript")] [] | f <- fs]
