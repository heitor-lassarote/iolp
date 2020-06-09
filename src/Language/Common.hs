module Language.Common where

import Universum

import Data.Aeson

type Variable varType = (Text, varType)

data ValueType varType
    = Variable Text
    | Constant varType
    deriving (Eq, Functor, Show)

instance (FromJSON varType) => FromJSON (ValueType varType) where
    parseJSON = withObject "value type" $ \o -> do
        variable <- o .:? "variable"
        constant <- o .:? "constant"
        case (variable, constant) of
            (Nothing, Nothing) -> fail "Must provide either a variable xor a constant."
            (Nothing, Just c ) -> Constant <$> parseJSON c
            (Just v , Nothing) -> pure $ Variable v
            (Just _ , Just _ ) -> fail "Must provide either a variable xor a constant."

instance (ToJSON varType) => ToJSON (ValueType varType) where
    toJSON (Variable text) = object [ "variable" .= String text ]
    toJSON (Constant var ) = object [ "constant" .= toJSON var  ]

data BinarySymbol
    -- Arithmetic
    = Add
    | Divide
    | Multiply
    | Subtract

    -- Comparison
    | Different
    | Equal
    | Greater
    | GreaterEqual
    | Less
    | LessEqual

    -- Logical
    | And
    | Or
    deriving (Eq, Ord, Show)

instance FromJSON BinarySymbol where
    parseJSON = withObject "BinarySymbol" $ \o -> toSymbol <$> o .: "symbol"
      where
        toSymbol = \case
            "Add"          -> Add
            "Divide"       -> Divide
            "Multiply"     -> Multiply
            "Subtract"     -> Subtract
            "Different"    -> Different
            "Equal"        -> Equal
            "Greater"      -> Greater
            "GreaterEqual" -> GreaterEqual
            "Less"         -> Less
            "LessEqual"    -> LessEqual
            "And"          -> And
            "Or"           -> Or
            other          -> error $ "Unknown symbol '" <> other <> "'."

-- TODO: Perhaps change from object to text?
instance ToJSON BinarySymbol where
    toJSON symbol = object [ "symbol" .= String (show symbol) ]

data UnarySymbol
    -- Arithmetic
    = Negate

    -- Logical
    | Not
    deriving (Eq, Ord, Show)

instance FromJSON UnarySymbol where
    parseJSON = withObject "UnarySymbol" $ \o -> toSymbol <$> o .: "symbol"
      where
        toSymbol = \case
            "Negate"       -> Negate
            "Not"          -> Not
            other          -> error $ "Unknown symbol '" <> other <> "'."

instance ToJSON UnarySymbol where
    toJSON symbol = object [ "symbol" .= String (show symbol) ]

data Symbol
    = UnarySymbol !UnarySymbol
    | BinarySymbol !BinarySymbol
    deriving (Eq, Show)
