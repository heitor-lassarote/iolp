module Foundation where

import Data.Foldable (traverse_)
import Universum hiding (get, traverse_, (^.))

import qualified Codec.Archive.Zip as Zip
import           Crypto.KDF.BCrypt
import           Data.Aeson
import           Database.Esqueleto hiding (Value)
import qualified Database.Persist                 as P
import           Network.HTTP.Types.Status
import           Network.Wai (requestHeaders)
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath
import qualified System.IO.Temp as Temp
import           Yesod hiding (delete, (==.))
import           Yesod.Core.Types (Logger)

import Config
import Database

import Language.Bundles

data LowCode = LowCode
    { appLogger      :: Logger
    , connectionPool :: ConnectionPool
    , serverConfig   :: ServerConfig
    }

instance Yesod LowCode

instance YesodPersist LowCode where
    type YesodPersistBackend LowCode = SqlBackend

    runDB action = runSqlPool action =<< getsYesod connectionPool

mkYesod "LowCode" [parseRoutes|
/editor                  EditorR  OPTIONS POST
/editor/#ProjectId       ProjectR GET OPTIONS PUT DELETE
/editor/#ProjectId/build BuildR   OPTIONS GET
/user                    UserR    OPTIONS GET POST
|]

data EditorPOST = EditorPOST
    { editorPOSTName :: Text
    , editorPOSTAst  :: BundleCssHtmlLogic
    }

instance FromJSON EditorPOST where
    parseJSON = withObject "Foundation.EditorPOST" \o ->
        EditorPOST <$> o .: "name"
                   <*> o .: "ast"

data UserPOST = UserPOST
    { userPOSTEmail    :: Text
    , userPOSTPassword :: Text
    }

instance FromJSON UserPOST where
    parseJSON = withObject "Foundation.UserPOST" \o ->
        UserPOST <$> o .: "email"
                 <*> o .: "password"

newtype UserGET = UserGET
    { userGETProjects :: [ProjectGET]
    }

instance ToJSON UserGET where
    toJSON UserGET {..} = object
        [ "projects" .= userGETProjects
        ]

data ProjectGET = ProjectGET
    { projectGETId   :: ProjectId
    , projectGETName :: Text
    }

instance ToJSON ProjectGET where
    toJSON ProjectGET {..} = object
        [ "id"   .= projectGETId
        , "name" .= projectGETName
        ]

authenticate :: Handler UserId
authenticate = maybe notAuthenticated validate' =<< lookupBasicAuth
  where
    validate' (email, password) = runDB do
        user <- getBy404 $ UniqueUserEmail email
        let pwd = encodeUtf8 password :: ByteString
            hash = userPassword $ entityVal user
        if validatePassword pwd hash
        then pure $ entityKey user
        else notFound  -- We don't return permission denied to not leak available emails.

addCorsHeaders :: Handler ()
addCorsHeaders = do
    req <- waiRequest
    let origin = requestHeaders req
                    & find ((== "Access-Control-Allow-Origin") . fst)
                    & fmap (decodeUtf8 . snd)
                    & fromMaybe "http://localhost:4200"
    addHeader "Access-Control-Allow-Origin" origin
    addHeader "Access-Control-Allow-Headers" "Content-Type, Authorization"
    addHeader "Access-Control-Allow-Credentials" "true"

emptyOptions :: Handler ()
emptyOptions = do
    addCorsHeaders
    sendResponse ("" :: Text)

optionsEditorR :: Handler ()
optionsEditorR = emptyOptions

postEditorR :: Handler Value
postEditorR = do
    addCorsHeaders
    userId <- authenticate
    post <- requireCheckJsonBody :: Handler EditorPOST
    let project = Project (toJSON $ editorPOSTAst post) (editorPOSTName post) userId
    runDB $ toJSON <$> insert400 project

getProjectR :: ProjectId -> Handler Value
getProjectR pid = do
    addCorsHeaders
    void authenticate
    runDB (toJSON . projectAst <$> get404 pid)

optionsProjectR :: ProjectId -> Handler ()
optionsProjectR _ = emptyOptions

putProjectR :: ProjectId -> Handler ()
putProjectR pid = do
    addCorsHeaders
    userId <- authenticate
    post <- requireCheckJsonBody :: Handler EditorPOST
    let project = Project (toJSON $ editorPOSTAst post) (editorPOSTName post) userId
    runDB $ P.repsert pid project
    sendResponse ("OK" :: Text)

deleteProjectR :: ProjectId -> Handler ()
deleteProjectR pid = do
    addCorsHeaders
    userId <- authenticate
    runDB $ delete $ from \project ->
        where_ (project ^. ProjectUserId ==. val userId &&. project ^. ProjectId ==. val pid)
    sendResponse ("OK" :: Text)

getBuildR :: ProjectId -> Handler Value
getBuildR pid = do
    addCorsHeaders
    void authenticate
    project <- runDB $ get404 pid
    let bundle = fromJSON $ projectAst project :: Result BundleCssHtmlLogic
    status <- case bundle of
        Error err -> sendResponseStatus status500 err
        Success bundle' -> pure $ generate bundle'
    case status of
        OK files -> mkBundleZip (toString $ projectName project) files
        LogicError errors -> sendResponseStatus status500 $ toJSON errors
        CodegenError errors -> sendResponseStatus status500 $ toJSON errors
  where
    mkBundleZip name files = do
        -- FIXME (maybe): Creates a temporary folder with name SqlBackendKey {unSqlBackendKey = ?}
        -- where ? is the actual PID.
        tempPath <- liftIO $ (</> show pid) <$> Temp.getCanonicalTemporaryDirectory
        liftIO $ createDirectoryIfMissing False tempPath
        traverse_ (uncurry (writeFile . (tempPath </>))) files
        selectors <- traverse (Zip.mkEntrySelector . fst) files
        let entries = zip (encodeUtf8 . snd <$> files) selectors
            file = tempPath </> name <> ".zip"
        Zip.createArchive file (traverse_ (uncurry (Zip.addEntry Zip.Store)) entries)
        sendFile "application/zip" file

optionsBuildR :: ProjectId -> Handler ()
optionsBuildR _ = emptyOptions

getUserR :: Handler Value
getUserR = do
    addCorsHeaders
    userId <- authenticate
    projects <- runDB $ select $ from \project -> do
        where_ (project ^. ProjectUserId ==. val userId)
        pure project
    returnJson $ UserGET $ map toGet projects
  where
    toGet project =
        let projectKey = entityKey project
            Project _ name _ = entityVal project
         in ProjectGET projectKey name

optionsUserR :: Handler ()
optionsUserR = emptyOptions

postUserR :: Handler Value
postUserR = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Headers" "Content-Type"

    userPOST <- requireCheckJsonBody :: Handler UserPOST
    pwd <- liftIO $ hashPassword 13 (encodeUtf8 $ userPOSTPassword userPOST :: ByteString)
    let user = User (userPOSTEmail userPOST) pwd
    runDB $ toJSON <$> insert400 user
