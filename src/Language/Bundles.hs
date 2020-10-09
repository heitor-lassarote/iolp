module Language.Bundles where

import Universum

import qualified Codec.Archive.Zip as Zip
import           Data.Aeson hiding (Error)
import           Data.Default.Class
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map.Merge
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

data PageCssHtmlLogic = PageCssHtmlLogic
    { css   :: !CSS.AST
    , html  :: ![HTML.AST]
    , logic :: ![L.Module ()]
    , name  :: !Name
    } deriving (Generic, FromJSON, ToJSON)

data BundleCssHtmlLogic = BundleCssHtmlLogic
    { cssOptions   :: !CSS.Options
    , extraJsFiles :: !(Map Name Text)
    , htmlOptions  :: !HTML.Options
    , jsOptions    :: !JS.Options
    , mainModule   :: !(L.Module ())
    , pages        :: ![PageCssHtmlLogic]
    } deriving (Generic, ToJSON)

instance FromJSON BundleCssHtmlLogic where
    parseJSON = withObject "Language.Bundles.BundleCssHtmlLogic" \o ->
        BundleCssHtmlLogic <$> o .:? "cssOptions"   .!= def
                           <*> o .:? "extraJsFiles" .!= Map.empty
                           <*> o .:? "htmlOptions"  .!= def
                           <*> o .:? "jsOptions"    .!= def
                           <*> o .:  "mainModule"
                           <*> o .:  "pages"

instance Bundle BundleCssHtmlLogic where
    generate BundleCssHtmlLogic {..}
        | null errors = Right files
        | otherwise   = Left  errors
      where
        logics = concatMap logic pages
        analysis = L.analyze (mainModule :| logics)

        errors = lefts result
        files = concat $ rights result
        -- FIXME: It's completely ignoring the generation of the main function!
        result = case analysis of
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

        extraJsFiles' = Map.toList extraJsFiles

        jsFiles :: (Emit gen, Monoid gen) => [GeneratedSuccess gen]
        jsFiles = fmap (\(path, file) -> GeneratedSuccess (emit file) (toString path) []) extraJsFiles'

        linkHtml
            :: (Emit gen, Monoid gen)
            => Map Name (L.Module L.Type)
            -> Map Name [L.Warning]
            -> PageCssHtmlLogic
            -> Either GeneratedFail [GeneratedSuccess gen]
        linkHtml allMods warnings page = do
            js' <- Map.elems <$> traverse logicResult modulesAndErrors
            css' <- uiResult cssName $ evalCodegenT (CSS.withOptions cssOptions) $ codegen $ css page
            html' <- uiResult htmlName $ evalCodegenT (HTML.withOptions htmlOptions) $ codegen $ HTML.link cssNames (map fst extraJsFiles' <> jsNames) $ html page
            pure (css' : html' : jsFiles <> js')
          where
            pName = name page
            cssName = pName <> ".css"
            cssNames = if css page == CSS.CSS [] then [] else [cssName]
            jsName = (<> ".js")
            jsNames = map fst3 $ Map.elems modulesAndErrors
            htmlName = pName <> ".html"
            uiResult name =
                bimap (\e -> GeneratedFail [e] (toString name) [])
                      (\f -> GeneratedSuccess f (toString name) [])
            logicResult (n, gen, w) =
                bimap (\e -> GeneratedFail e (toString $ jsName n) w)
                      (\f -> GeneratedSuccess f (toString $ jsName n) w)
                      gen
            fst3 (a, _, _) = a
            genLogic = evalCodegenT (JS.withOptions jsOptions) . codegen . (convert :: L.Module L.Type -> JS.Module)
            modulesAndErrors =
                Map.Merge.merge
                    (Map.Merge.mapMissing \n m -> (n, first pure $ genLogic m, []))
                    (Map.Merge.dropMissing)
                    (Map.Merge.zipWithMatched \n m w -> (n, first pure $ genLogic m, w))
                    (Map.filterWithKey (\k _ -> k `elem` map L.moduleName (logic page)) allMods)
                    (Map.map (map L.prettyWarning) warnings)
