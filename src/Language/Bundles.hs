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
    } deriving (Generic, FromJSON, ToJSON)

data BundleCssHtmlLogic = BundleCssHtmlLogic
    { cssOptions       :: CSS.Options
    , htmlOptions      :: HTML.Options
    , jsOptions        :: JS.Options
    , logicEnvironment :: L.Environment
    , pages            :: [PageCssHtmlLogic]
    } deriving (Generic, ToJSON)

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
        analysis = L.execAnalyzerT L.emptyState $ L.analyzeMany logicEnvironment logics
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
    parseJSON = withObject "Language.Bundles.BundleCssLogicUi" \o ->
        BundleCssHtmlLogic <$> o .:? "cssOptions"       .!= CSS.defaultOptions
                           <*> o .:? "htmlOptions"      .!= HTML.defaultOptions
                           <*> o .:? "jsOptions"        .!= JS.defaultOptions
                           <*> o .:? "logicEnvironment" .!= L.Environment Map.empty
                           <*> o .:  "pages"
