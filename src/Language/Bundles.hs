{-# LANGUAGE RankNTypes #-}

module Language.Bundles where

import Prelude (foldr)
import Universum hiding (foldr)

import           Data.Aeson
import qualified Data.Map as Map

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
    | LogicError [Text]
    | CodegenError [(FilePath, Text)]
    deriving (Eq, Show)

class Bundle bundle where
    generate :: (Emit gen, Monoid gen) => bundle -> Status gen

data PageCssHtmlLogic = PageCssHtmlLogic
    { css   :: CSS.AST
    , html  :: HTML.AST
    , logic :: [L.AST]
    , name  :: Name
    } deriving (Generic)

instance FromJSON PageCssHtmlLogic
instance ToJSON   PageCssHtmlLogic

data BundleCssHtmlLogic = BundleCssHtmlLogic
    { cssOptions    :: CSS.Options
    , htmlOptions   :: HTML.Options
    , jsOptions     :: JS.Options
    , logicMetadata :: L.Metadata
    , pages         :: [PageCssHtmlLogic]
    } deriving (Generic)

instance Bundle BundleCssHtmlLogic where
    generate BundleCssHtmlLogic {..}
        | null (L.errors analysis) && null errors = OK $ concatMap snd files
        | null (L.errors analysis) = CodegenError errors
        | otherwise = LogicError $ show <$> L.errors analysis
      where
        --csss :: (Emit gen, Monoid gen) => ([(Name, Text)], [(Name, gen)])
        csss     = mapWithName (evalCodegenT (CSS.withOptions cssOptions) . codegen . css) pages
        htmls    = mapWithName (evalCodegenT (HTML.withOptions htmlOptions) . codegen . html) pages
        jss      = mapWithName (evalCodegenT (JS.withOptions jsOptions) . codegen . L.lToJs . logic) pages
        analysis = L.execAnalyzerT L.emptyState $ L.analyzeMany logicMetadata logics
        files    = [withExtension ".css" csss, withExtension ".html" htmls, withExtension ".js" jss]
        errors   = concatMap fst files

        logics = concatMap logic pages

        partitionEither = foldr go ([], [])
          where
            go (name, Left  l) (ls, rs) = ((name, l) : ls, rs)
            go (name, Right r) (ls, rs) = (ls, (name, r) : rs)

        mapWithName f = partitionEither . map (toString . name &&& f)

        withExtension
            :: FilePath
            -> ([(FilePath, a)], [(FilePath, b)])
            -> ([(FilePath, a)], [(FilePath, b)])
        withExtension e = bimap mapFirst mapFirst
          where
            mapFirst :: [(FilePath, c)] -> [(FilePath, c)]
            mapFirst [] = []
            mapFirst ((x, y) : rest) = (x <> e, y) : mapFirst rest

instance FromJSON BundleCssHtmlLogic where
    parseJSON = withObject "BundleCssLogicUi" \o ->
        BundleCssHtmlLogic <$> o .:? "cssOptions"    .!= CSS.defaultOptions
                           <*> o .:? "htmlOptions"   .!= HTML.defaultOptions
                           <*> o .:? "jsOptions"     .!= JS.defaultOptions
                           <*> o .:? "logicMetadata" .!= L.Metadata Map.empty
                           <*> o .:  "pages"

instance ToJSON BundleCssHtmlLogic

--data BundleFile contents = BundleFile
--    { contents :: contents
--    , name     :: Name
--    } deriving (Functor, Generic)

-- instance (FromJSON ast) => FromJSON (BundleFile ast)
-- instance (ToJSON   ast) => ToJSON   (BundleFile ast)

--data BundleCssHtmlLogic = BundleCssHtmlLogic
--    { css           :: [BundleFile CSS.AST]
--    , cssOptions    :: CSS.Options
--    , html          :: [BundleFile HTML.AST]
--    , htmlOptions   :: HTML.Options
--    , logic         :: [BundleFile L.AST]
--    , logicMetadata :: L.Metadata
--    , jsOptions     :: JS.Options
--    } deriving (Generic)

--instance Bundle BundleCssHtmlLogic where
--    generate BundleCssHtmlLogic {..}
--        | null errors =
--            Right $ concatMap rights [withExtension ".css" csss, withExtension ".html" htmls, withExtension ".js" [js]]
--        | otherwise   = Left errors
--      where
--        csss     = map (evalCodegenT (CSS.withOptions cssOptions) . codegen . ast) css
--        htmls    = map (evalCodegenT (HTML.withOptions htmlOptions) . codegen . ast) html
--        analysis = L.execAnalyzerT L.emptyState $ L.analyzeMany logicMeta $ ast logic
--        js       = evalCodegenT (JS.withOptions jsOptions) $ codegen $ L.lToJs $ ast logic
--        errors   = (show <$> L.errors analysis) ++ concatMap lefts [csss, htmls, [js]]

--        withExtension e = (fmap . map) (,e)

--instance FromJSON BundleCssHtmlLogic where
--    parseJSON = withObject "BundleCssLogicUi" \o ->
--        BundleCssHtmlLogic <$> o .:  "css"
--                           <*> o .:? "cssOptions"  .!= CSS.defaultOptions
--                           <*> o .:  "html"
--                           <*> o .:? "htmlOptions" .!= HTML.defaultOptions
--                           <*> o .:  "logic"
--                           <*> o .:  "logicMetadata"
--                           <*> o .:? "jsOptions"   .!= JS.defaultOptions

--instance ToJSON BundleCssHtmlLogic
