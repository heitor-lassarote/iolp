module Foundation where

import Universum hiding (get, (^.))

import           Crypto.KDF.BCrypt
import           Data.Aeson
import           Database.Esqueleto hiding (Value)
import qualified Database.Persist as P
import           Network.HTTP.Types.Status
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
/editor                  EditorR  POST
/editor/#ProjectId       ProjectR GET PUT DELETE
/editor/#ProjectId/build BuildR   GET
/user                    UserR    GET POST
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

postEditorR :: Handler Value
postEditorR = do
    userId <- authenticate
    post <- requireCheckJsonBody :: Handler EditorPOST
    let project = Project (toJSON $ editorPOSTAst post) (editorPOSTName post) userId
    runDB $ toJSON <$> insert400 project

getProjectR :: ProjectId -> Handler Value
getProjectR pid = do
    void authenticate
    runDB (toJSON . projectAst <$> get404 pid)

putProjectR :: ProjectId -> Handler ()
putProjectR pid = do
    userId <- authenticate
    post <- requireCheckJsonBody :: Handler EditorPOST
    let project = Project (toJSON $ editorPOSTAst post) (editorPOSTName post) userId
    runDB $ P.repsert pid project
    sendResponse ("OK" :: Text)

deleteProjectR :: ProjectId -> Handler ()
deleteProjectR pid = do
    userId <- authenticate
    runDB $ delete $ from \project ->
        where_ (project ^. ProjectUserId ==. val userId &&. project ^. ProjectId ==. val pid)
    sendResponse ("OK" :: Text)

getBuildR :: ProjectId -> Handler Value
getBuildR pid = do
    void authenticate
    project <- runDB $ get404 pid
    let bundle = fromJSON $ projectAst project :: Result BundleCssHtmlLogic
    status <- case bundle of
        Error err -> sendResponseStatus status400 err
        Success bundle' -> pure $ generate bundle'
    case status of
        Right files -> do
            zipPath <- mkBundleZip (show $ fromSqlKey pid) (toString $ projectName project) $ map toTuple files
            sendFile "application/zip" zipPath
        Left errors -> sendResponseStatus status400 $ toJSON errors
  where
    toTuple GeneratedSuccess {..} = (path, file)

getUserR :: Handler Value
getUserR = do
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

postUserR :: Handler Value
postUserR = do
    userPOST <- requireCheckJsonBody :: Handler UserPOST
    pwd <- liftIO $ hashPassword 13 (encodeUtf8 $ userPOSTPassword userPOST :: ByteString)
    let user = User (userPOSTEmail userPOST) pwd
    runDB $ toJSON <$> insert400 user
