module Language.Bundles where

import Universum

import qualified Codec.Archive.Zip as Zip
import           Data.Aeson
import           Data.Default.Class
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath
import qualified System.IO.Temp as Temp

import           Language.Codegen
import           Language.Common (Name)
import qualified Language.CSS as CSS
import           Language.Emit
import           Language.LanguageConverter
import qualified Language.HTML as HTML
import qualified Language.JavaScript as JS
import qualified Language.LowCode.Logic as L

data Status gen
    = OK [(FilePath, gen)]
    -- TODO: In the future, unify these two error types.
    | LogicError [Text]
    | CodegenError [(FilePath, Text)]
    deriving (Eq, Show)

class Bundle bundle where
    generate :: (Emit gen, Monoid gen) => bundle -> Status gen

mkBundleZip
    :: (MonadIO m, MonadThrow m)
    => FilePath
    -> FilePath
    -> [(FilePath, Text)]
    -> m FilePath
mkBundleZip tempFolderName name files = do
    tempPath <- liftIO $ (</> tempFolderName) <$> Temp.getCanonicalTemporaryDirectory
    liftIO $ createDirectoryIfMissing False tempPath
    traverse_ (uncurry (writeFile . (tempPath </>))) files
    selectors <- traverse (Zip.mkEntrySelector . fst) files
    let entries = zip (encodeUtf8 . snd <$> files) selectors
        file = tempPath </> name <> ".zip"
    Zip.createArchive file (traverse_ (uncurry (Zip.addEntry Zip.Store)) entries)
    pure file

data PageCssHtmlLogic = PageCssHtmlLogic
    { css   :: CSS.AST
    , html  :: [HTML.AST]
    , logic :: [L.TopLevel () L.Metadata]
    , name  :: Name
    } deriving (Generic, FromJSON, ToJSON)

data BundleCssHtmlLogic = BundleCssHtmlLogic
    { cssOptions       :: CSS.Options
    , extraJsFiles     :: [(Name, Text)]
    , htmlOptions      :: HTML.Options
    , jsOptions        :: JS.Options
    , logicEnvironment :: L.Environment
    , pages            :: [PageCssHtmlLogic]
    } deriving (Generic, ToJSON)

instance FromJSON BundleCssHtmlLogic where
    parseJSON = withObject "Language.Bundles.BundleCssHtmlLogic" \o ->
        BundleCssHtmlLogic <$> o .:? "cssOptions"       .!= def
                           <*> o .:? "extraJsFiles"     .!= []
                           <*> o .:? "htmlOptions"      .!= def
                           <*> o .:? "jsOptions"        .!= def
                           <*> o .:? "logicEnvironment" .!= def
                           <*> o .:  "pages"

instance Bundle BundleCssHtmlLogic where
    generate BundleCssHtmlLogic {..} = case analysis of
        Nothing -> undefined
        Just analysis'
            | null (L.errors $ snd analysis') && null errors -> OK files
            | null (L.errors $ snd analysis') -> CodegenError errors
            | otherwise -> LogicError $ L.prettyError <$> L.errors (snd analysis')
      where
        logics = concatMap logic pages
        analysis = L.evalAnalyzer' $ L.analyzeMany logicEnvironment logics

        result = map linkHtml pages
        errors = lefts result
        files = join $ rights result

        jsFiles :: (Emit gen, Monoid gen) => [(String, gen)]
        jsFiles = bimap toString emit <$> extraJsFiles

        linkHtml :: (Emit gen, Monoid gen) => PageCssHtmlLogic -> Either (String, Text) [(String, gen)]
        linkHtml page = do
            js' <- withName jsName $ evalCodegenT (JS.withOptions jsOptions) $ codegen $ (convert $ logic page :: JS.AST)
            css' <- withName cssName $ evalCodegenT (CSS.withOptions cssOptions) $ codegen $ css page
            html' <- withName htmlName $ evalCodegenT (HTML.withOptions htmlOptions) $ codegen $ HTML.link cssNames (map fst extraJsFiles <> jsNames) $ html page
            pure (jsFiles <> [js', css', html'])
          where
            pName = name page
            cssName = pName <> ".css"
            cssNames = if css page == CSS.CSS [] then [] else [cssName]
            jsName = pName <> ".js"
            jsNames = if null (logic page) then [] else [jsName]
            htmlName = pName <> ".html"
            withName name = bimap (toString name,) (toString name,)
