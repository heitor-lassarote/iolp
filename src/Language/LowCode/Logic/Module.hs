module Language.LowCode.Logic.Module
    ( Module (..)
    , mkModule
    , parseModule
    ) where

import Universum hiding (Type, many, try)

import           Data.Aeson
import qualified Data.Map.Strict as Map
import           Text.Megaparsec
import           Text.Megaparsec.Char

import Language.Common (Name)
import Language.LowCode.Logic.AST (Constructor, Function, function)
import Language.LowCode.Logic.Parser
import Language.LowCode.Logic.Type (Constructor (..), Type)

data Module exprMetadata astMetadata = Module
    { adtTemplates    :: !(Map Name [Constructor Type])
    , externs         :: !(Map Name Type)
    , functions       :: ![Function exprMetadata astMetadata]
    , importedModules :: ![Name]
    , moduleName      :: !Name
    } deriving (Eq, Generic, Show, ToJSON)

instance (FromJSON astMetadata) => FromJSON (Module () astMetadata) where
    parseJSON = withObject "Language.LowCode.Logic.Module.Module" \o ->
        Module <$> o .: "adtTemplates"
               <*> o .: "externs"
               <*> o .: "functions"
               <*> o .: "importedModules"
               <*> o .: "moduleName"

mkModule :: Name -> Module exprMetadata astMetadata
mkModule name = Module Map.empty Map.empty [] [] name

parseModule :: Text -> Either Text (Module () ())
parseModule = parse' module'

module' :: Parser (Module () ())
module' = do
    name <- label "module name" (symbol "module" *> variableName)
    imports' <- imports
    (adts, exts, funcs) <- topLevelDeclarations
    pure $ Module adts exts funcs imports' name

imports :: Parser [Name]
imports = label "module imports" (many import')

import' :: Parser Name
import' = label "module import" (symbol "import" *> variableName <* endl)

data Decl
    = AlgebraicDecl (Name, [Constructor Type])
    | ExternDecl (Type, Name)
    | FunctionDecl (Function () ())

topLevelDeclarations :: Parser (Map Name [Constructor Type], Map Name Type, [Function () ()])
topLevelDeclarations = label "top level declaration" do
    decls <- many $ choice [AlgebraicDecl <$> adt, ExternDecl <$> try extern, FunctionDecl <$> function]
    pure $ partitionDecls decls
  where
    partitionDecls = foldr (flip go) (Map.empty, Map.empty, [])
    go (adts, exts, funcs) = \case
        AlgebraicDecl (name, ctors) -> (Map.insert name ctors adts, exts, funcs)
        ExternDecl (type', name) -> (adts, Map.insert name type' exts, funcs)
        FunctionDecl func -> (adts, exts, func : funcs)

adt :: Parser (Name, [Constructor Type])
adt = label "algebraic data type definition" do
    name <- adtName
    constructors <- adtConstructors name
    pure (name, constructors)
  where
    adtName = label "algebraic data type name" (symbol "type" *> variableName)
    adtConstructors name = label "constructor definition" $ brackets $ flip sepEndBy (lexeme (char ',')) $
        liftA2 (Constructor name) variableName (optional $ parenthesis $ typeName)

extern :: Parser (Type, Name)
extern = label "extern declaration" (liftA2 (,) (symbol "extern" *> typeName) variableName <* endl)
