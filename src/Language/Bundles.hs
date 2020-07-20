module Language.Bundles where

import Universum

import           Data.Aeson
import           Data.Default.Class

import           Language.Codegen
import           Language.Common (Name)
import qualified Language.CSS as CSS
import           Language.Emit
import qualified Language.HTML as HTML
import qualified Language.JavaScript as JS
import qualified Language.LowCode.Logic.JavaScriptConverter as L
import qualified Language.LowCode.Logic as L

data Status gen
    = OK [(FilePath, gen)]
    -- TODO: In the future, unify these two error types.
    | LogicError [Text]
    | CodegenError [(FilePath, Text)]
    deriving (Eq, Show)

class Bundle bundle where
    generate :: (Emit gen, Monoid gen) => bundle -> Status gen

data PageCssHtmlLogic = PageCssHtmlLogic
    { css   :: CSS.AST
    , html  :: [HTML.AST]
    , logic :: [L.AST]
    , name  :: Name
    } deriving (Generic, FromJSON, ToJSON)

data BundleCssHtmlLogic = BundleCssHtmlLogic
    { cssOptions       :: CSS.Options
    , htmlOptions      :: HTML.Options
    , jsOptions        :: JS.Options
    , logicEnvironment :: L.Environment
    , pages            :: [PageCssHtmlLogic]
    } deriving (Generic, ToJSON)

instance FromJSON BundleCssHtmlLogic where
    parseJSON = withObject "Language.Bundles.BundleCssHtmlLogic" \o ->
        BundleCssHtmlLogic <$> o .:? "cssOptions"       .!= def
                           <*> o .:? "htmlOptions"      .!= def
                           <*> o .:? "jsOptions"        .!= def
                           <*> o .:? "logicEnvironment" .!= def
                           <*> o .:  "pages"

instance Bundle BundleCssHtmlLogic where
    generate BundleCssHtmlLogic {..}
        | null (L.errors analysis) && null errors = OK files
        | null (L.errors analysis) = CodegenError errors
        | otherwise = LogicError $ L.prettyError <$> L.errors analysis
      where
        logics = concatMap logic pages
        analysis = snd $ L.evalAnalyzer' $ L.analyzeMany logicEnvironment logics

        result = map linkHtml pages
        errors = lefts result
        files = join $ rights result

        linkHtml page = do
            let pName = name page
                cssName = pName <> ".css"
                cssNames = if css page == CSS.CSS [] then [] else [cssName]
                jsName = pName <> ".js"
                jsNames = if null (logic page) then [] else [jsName]
                htmlName = pName <> ".html"
                withName name = bimap (toString name,) (toString name,)
            js' <- withName jsName $ evalCodegenT (JS.withOptions jsOptions) $ codegen $ L.lToJs $ logic page
            css' <- withName cssName $ evalCodegenT (CSS.withOptions cssOptions) $ codegen $ css page
            html' <- withName htmlName $ evalCodegenT (HTML.withOptions htmlOptions) $ codegen $ HTML.link cssNames jsNames $ html page
            pure [js', css', html']
