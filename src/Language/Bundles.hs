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
                jsName = pName <> ".js"
                htmlName = pName <> ".html"
                withName name = bimap (toString name,) (toString name,)
            js' <- withName jsName $ evalCodegenT (JS.withOptions jsOptions) $ codegen $ L.lToJs $ logic page
            css' <- withName cssName $ evalCodegenT (CSS.withOptions cssOptions) $ codegen $ css page
            html' <- withName htmlName $ evalCodegenT (HTML.withOptions htmlOptions) $ codegen $ HTML.link [cssName] [jsName] $ html page
            pure [js', css', html']

--instance Bundle BundleCssHtmlLogic where
--    generate BundleCssHtmlLogic {..}
--        | null (L.errors analysis) && null errors = OK $ concatMap snd files
--        | null (L.errors analysis) = CodegenError errors
--        | otherwise = LogicError $ L.prettyError <$> L.errors analysis
--      where
--        --csss :: (Emit gen, Monoid gen) => ([(Name, Text)], [(Name, gen)])
--        csss     = mapWithName (evalCodegenT (CSS.withOptions cssOptions) . codegen . css) pages
--        jss      = mapWithName (evalCodegenT (JS.withOptions jsOptions) . codegen . L.lToJs . logic) pages
--        htmls    = mapWithName (evalCodegenT (HTML.withOptions htmlOptions) . codegen . html) pages
--        analysis = L.execAnalyzer def $ L.analyzeMany logicEnvironment logics
--        files    = [withExtension ".css" csss, withExtension ".html" htmls, withExtension ".js" jss]
--        errors   = concatMap fst files
--
--        logics = concatMap logic pages
--
--        partitionEither = foldr go ([], [])
--          where
--            go (name, Left  l) (ls, rs) = ((name, l) : ls, rs)
--            go (name, Right r) (ls, rs) = (ls, (name, r) : rs)
--
--        mapWithName f = partitionEither . map (toString . name &&& f)
--
--        withExtension
--            :: FilePath
--            -> ([(FilePath, a)], [(FilePath, b)])
--            -> ([(FilePath, a)], [(FilePath, b)])
--        withExtension e = bimap mapFirst mapFirst
--          where
--            mapFirst :: [(FilePath, c)] -> [(FilePath, c)]
--            mapFirst [] = []
--            mapFirst ((x, y) : rest) = (x <> e, y) : mapFirst rest

instance FromJSON BundleCssHtmlLogic where
    parseJSON = withObject "Language.Bundles.BundleCssLogicUi" \o ->
        BundleCssHtmlLogic <$> o .:? "cssOptions"       .!= def
                           <*> o .:? "htmlOptions"      .!= def
                           <*> o .:? "jsOptions"        .!= def
                           <*> o .:? "logicEnvironment" .!= def
                           <*> o .:  "pages"
