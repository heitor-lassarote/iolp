module Language.Bundles where

import Universum

import qualified Codec.Archive.Zip as Zip
import           Data.Aeson hiding (Error)
import           Data.Default.Class
import qualified Data.Map.Strict as Map
import           Data.These
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

data Status e a
    = Ok  e
    | Err a
    deriving (Eq, Show)

class Bundle bundle where
    generate :: (Emit gen, Monoid gen) => bundle -> Status [(FilePath, gen)] [(FilePath, Text)]

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
    , logic :: [L.Module () L.Metadata]
    , name  :: Name
    } deriving (Generic, FromJSON, ToJSON)

data BundleCssHtmlLogic = BundleCssHtmlLogic
    { cssOptions   :: CSS.Options
    , extraJsFiles :: [(Name, Text)]
    , htmlOptions  :: HTML.Options
    , jsOptions    :: JS.Options
    , mainModule   :: L.Module () L.Metadata
    , pages        :: [PageCssHtmlLogic]
    } deriving (Generic, ToJSON)

instance FromJSON BundleCssHtmlLogic where
    parseJSON = withObject "Language.Bundles.BundleCssHtmlLogic" \o ->
        BundleCssHtmlLogic <$> o .:? "cssOptions"   .!= def
                           <*> o .:? "extraJsFiles" .!= []
                           <*> o .:? "htmlOptions"  .!= def
                           <*> o .:? "jsOptions"    .!= def
                           <*> o .:  "mainModule"
                           <*> o .:  "pages"

instance Bundle BundleCssHtmlLogic where
    generate BundleCssHtmlLogic {..}
        | null errors = Ok files
        | otherwise   = Err errors
      where
        logics = concatMap logic pages
        analysis = L.evalAnalyzer (L.analyze logics) (L.AnalyzerReader mainModule L.unitType) def

        errors = lefts result
        files = join $ rights result
        -- FIXME: It's completely ignoring the generation of the main function!
        result = case analysis of
            This writer       -> mkLeft writer
            That (_, imports) -> linkHtml imports <$> pages
            -- FIXME: It may simply return warnings.
            These writer _    -> mkLeft writer
          where
            mkLeft = map (Left . first (toString . L.prettyError) . swap) . L.errors

        jsFiles :: (Emit gen, Monoid gen) => [(String, gen)]
        jsFiles = bimap toString emit <$> extraJsFiles

        linkHtml
            :: (Emit gen, Monoid gen)
            => Map Name (L.Module L.Type a)
            -> PageCssHtmlLogic
            -> Either (String, Text) [(String, gen)]
        linkHtml allMods page = do
            let mods = (allMods Map.!) . L.moduleName <$> logic page
                jsNames = jsName . L.moduleName <$> mods
                logicGens = evalCodegenT (JS.withOptions jsOptions) . codegen . (convert :: L.Module L.Type a -> JS.Module) <$> mods
            js' <- sequence $ zipWith withName jsNames logicGens
            css' <- withName cssName $ evalCodegenT (CSS.withOptions cssOptions) $ codegen $ css page
            html' <- withName htmlName $ evalCodegenT (HTML.withOptions htmlOptions) $ codegen $ HTML.link cssNames (map fst extraJsFiles <> jsNames) $ html page
            pure (css' : html' : jsFiles <> js')
          where
            pName = name page
            cssName = pName <> ".css"
            cssNames = if css page == CSS.CSS [] then [] else [cssName]
            jsName = (<> ".js")
            htmlName = pName <> ".html"
            withName name = bimap (toString name,) (toString name,)
