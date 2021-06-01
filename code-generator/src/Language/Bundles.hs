module Language.Bundles
    ( GeneratedSuccess (..)
    , GeneratedFail (..)
    , Bundle (..)
    , mkBundleZip
    , PageCssHtmlLogic (..)
    , BundleCssHtmlLogic (..)
    ) where

import           Universum

import qualified Codec.Archive.Zip as Zip
import           Data.Aeson hiding (Error)
import           Data.Default.Class
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map.Merge
import qualified Data.Text.IO as Text.IO
import           Data.These
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath
import qualified System.IO.Temp as Temp
import qualified System.IO.Unsafe as Unsafe

import           Language.Codegen
import           Language.Common (Name)
import qualified Language.CSS as CSS
import           Language.Emit
import           Language.LanguageConverter
import qualified Language.HTML as HTML
import qualified Language.JavaScript as JS
import qualified Language.LowCode.Logic as L
import qualified Language.LowCode.Logic.Standard as L

data GeneratedSuccess gen = GeneratedSuccess
    { file     :: !gen
    , path     :: !FilePath
    , warnings :: ![Text]
    } deriving (Show, Generic, FromJSON, ToJSON)

data GeneratedFail = GeneratedFail
    { errors   :: ![Text]
    , path     :: !FilePath
    , warnings :: ![Text]
    } deriving (Show, Generic, FromJSON, ToJSON)

class Bundle bundle where
    generate :: (Emit gen, Monoid gen) => bundle -> Either [GeneratedFail] [GeneratedSuccess gen]

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

jsBitsDir :: FilePath
jsBitsDir = "jsbits"

inJsBitsDir :: FilePath -> FilePath
inJsBitsDir = (jsBitsDir </>)

data PageCssHtmlLogic = PageCssHtmlLogic
    { css   :: !CSS.AST
    , html  :: ![HTML.AST]
    , logic :: ![L.Module ()]
    , name  :: !Name
    } deriving (Generic, FromJSON, ToJSON)

data BundleCssHtmlLogic = BundleCssHtmlLogic
    { cssOptions      :: !CSS.Options
    , extraJsFiles    :: !(Map Name Text)
    , htmlOptions     :: !HTML.Options
    , includeStandard :: !Bool
    , jsOptions       :: !JS.Options
    , pages           :: ![PageCssHtmlLogic]
    } deriving (Generic, ToJSON)

instance FromJSON BundleCssHtmlLogic where
    parseJSON = withObject "Language.Bundles.BundleCssHtmlLogic" \o ->
        BundleCssHtmlLogic <$> o .:? "cssOptions"      .!= def
                           <*> o .:? "extraJsFiles"    .!= Map.empty
                           <*> o .:? "htmlOptions"     .!= def
                           <*> o .:? "includeStandard" .!= True
                           <*> o .:? "jsOptions"       .!= def
                           <*> o .:  "pages"

instance Bundle BundleCssHtmlLogic where
    generate BundleCssHtmlLogic {..}
        | null errors = Right files
        | otherwise   = Left  errors
      where
        errors = lefts result
        files = concat $ rights result
        logics
            | includeStandard = userLogic <> Map.elems L.standardModules
            | otherwise       = userLogic
          where
            userLogic = concatMap logic pages
        result = case L.analyze logics of
            This writer  -> mkLeft writer
            That imports -> linkHtml imports Map.empty <$> pages
            These writer imports
                | null (L.errors writer) -> linkHtml imports (L.warnings writer) <$> pages
                | otherwise              -> mkLeft writer
          where
            mkLeft = map (\(n, (es, ws)) -> go (toString n) es ws) . Map.toList . errorsAndWarnings
              where
                go n es ws = Left (GeneratedFail (L.prettyError <$> es) n (L.prettyWarning <$> ws))
            errorsAndWarnings writer =
                Map.Merge.merge
                    (Map.Merge.mapMissing \_ -> (,[]))
                    (Map.Merge.mapMissing \_ -> ([],))
                    (Map.Merge.zipWithMatched \_ -> (,))
                    (L.errors writer)
                    (L.warnings writer)

        extraJsFiles' = standard <> Map.toList extraJsFiles
          where
            -- TODO: Dear mr. H. B. Curry, please forgive me. My time is running out.
            unsafeReadFile = Unsafe.unsafePerformIO . Text.IO.readFile

            standard = map (id &&& unsafeReadFile . inJsBitsDir . toString)
                [ "jquery-3.5.1.min.js"
                , "LowCode.js"
                ]

        jsFiles :: (Emit gen, Monoid gen) => [GeneratedSuccess gen]
        jsFiles = fmap (\(path, file) -> GeneratedSuccess (emit file) (toString path) []) extraJsFiles'

        linkHtml
            :: (Emit gen, Monoid gen)
            => Map Name (L.AnalyzedModule)
            -> Map Name [L.Warning]
            -> PageCssHtmlLogic
            -> Either GeneratedFail [GeneratedSuccess gen]
        linkHtml allMods warnings page = do
            js' <- Map.elems <$> traverse logicResult modulesAndErrors
            css' <- uiResult cssName $ evalCodegenT (CSS.withOptions cssOptions) $ codegen $ css page
            html' <- uiResult htmlName
                $ evalCodegenT (HTML.withOptions htmlOptions)
                $ codegen
                $ HTML.link pName cssNames (map fst extraJsFiles' <> jsNames)
                $ html page
            pure (css' : html' : jsFiles <> js')
          where
            pName = name page
            cssName = pName <> ".css"
            cssNames = if css page == CSS.CSS [] then [] else [cssName]
            jsName = (<> ".js")
            jsNames = map (jsName . view _2) $ sortOn (view _4) $ Map.elems modulesAndErrors
            htmlName = pName <> ".html"

            uiResult name =
                bimap (\e -> GeneratedFail [e] (toString name) [])
                      (\f -> GeneratedSuccess f (toString name) [])
            logicResult (_includeNumber, n, gen, w) =
                bimap (\e -> GeneratedFail e (toString $ jsName n) w)
                      (\f -> GeneratedSuccess f (toString $ jsName n) w)
                      gen
            genLogic =
                evalCodegenT (JS.withOptions jsOptions)
                . codegen
                . (convertDef :: L.Module L.Type -> JS.Module)
                . L.amRootModule

            modulesAndErrors =
                Map.Merge.merge
                    (Map.Merge.mapMissing \n m -> (L.amIncludeNumber m, n, first pure $ genLogic m, []))
                    (Map.Merge.dropMissing)
                    (Map.Merge.zipWithMatched \n m w -> (L.amIncludeNumber m, n, first pure $ genLogic m, w))
                    (Map.filterWithKey (\k _ -> k `elem` map L.moduleName (logic page)) allMods)
                    (Map.map (map L.prettyWarning) warnings)
