module Language.LowCode.Logic.Standard.REST
    ( moduleRest
    ) where

import Universum

import Text.RawString.QQ

import Language.LowCode.Logic.Module
import Utility (unsafeRight)

moduleRest :: Module ()
moduleRest = unsafeRight $ parseModule moduleRestDef

moduleRestDef :: Text
moduleRestDef = [r|
module REST;

import Prelude;

noBefore() -> Unit {}

noSuccess(Text x) -> Unit {}

noError(Integer x, Text y, Text z) -> Unit {}

noComplete() -> Unit {}

extern (Text,       () -> Unit, Text -> Unit, (Integer, Text, Text) -> Unit, () -> Unit) -> {isSuccess: Bool, result: Text} GET;
extern (Text, Text, () -> Unit, Text -> Unit, (Integer, Text, Text) -> Unit, () -> Unit) -> {isSuccess: Bool, result: Text} POST;
|]
