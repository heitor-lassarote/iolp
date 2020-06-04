module Foundation where

import Universum hiding (get, (^.))

--import qualified Codec.Archive.Zip as Zip
import           Crypto.KDF.BCrypt
import           Data.Aeson
import           Database.Esqueleto hiding (Value)
import qualified Database.Persist                 as P
import           Network.HTTP.Types.Status
--import qualified System.IO.Temp as Temp
import           Yesod hiding (delete, (==.))

import Database
import Language.Bundles

data LowCode = LowCode
    { connectionPool :: ConnectionPool
    }

instance Yesod LowCode

instance YesodPersist LowCode where
    type YesodPersistBackend LowCode = SqlBackend

    runDB action = runSqlPool action =<< getsYesod connectionPool

mkYesod "LowCode" [parseRoutes|
/editor                  EditorR  POST
/editor/#ProjectId       ProjectR GET PUT DELETE
/editor/#ProjectId/build BuildR   GET
/user                    UserR    POST
|]

data EditorPOST = EditorPOST
    { editorPOSTName :: Text
    , editorPOSTAst  :: BundleCssLogicUi
    }

instance FromJSON EditorPOST where
    parseJSON = withObject "EditorPOST" $ \o ->
        EditorPOST <$> o .: "name"
                   <*> o .: "ast"

data UserPOST = UserPOST
    { userPOSTEmail    :: Text
    , userPOSTPassword :: Text
    }

instance FromJSON UserPOST where
    parseJSON = withObject "UserPOST" $ \o ->
        UserPOST <$> o .: "email"
                 <*> o .: "password"

data ProjectGET = ProjectGET
    { projectGETId   :: ProjectId
    , projectGETName :: Text
    }

instance ToJSON ProjectGET where
    toJSON ProjectGET {..} = object
        [ "id"   .= projectGETId
        , "name" .= projectGETName
        ]

data UserGET = UserGET
    { userGETEmail    :: Text
    , userGETProjects :: [Text]
    }

instance ToJSON UserGET where
    toJSON UserGET {..} = object
        [ "emai"     .= userGETEmail
        , "projects" .= userGETProjects
        ]

authenticate :: Handler UserId
authenticate = maybe notAuthenticated validate' =<< lookupBasicAuth
  where
    validate' (email, password) = runDB $ do
        user <- getBy404 $ UniqueUserEmail email
        let pwd = encodeUtf8 password :: ByteString
            hash = userPassword $ entityVal user
        if validatePassword pwd hash
        then pure $ entityKey user
        else permissionDenied ("Invalid password." :: Text)

postEditorR :: Handler ()
postEditorR = do
    userId <- authenticate
    post <- requireCheckJsonBody :: Handler EditorPOST
    let project = Project (decodeUtf8 $ encode $ editorPOSTAst post) (editorPOSTName post) userId
    runDB $ insert400_ project

getProjectR :: ProjectId -> Handler Text
getProjectR pid = authenticate *> runDB (projectAst <$> get404 pid)

putProjectR :: ProjectId -> Handler ()
putProjectR pid = do
    userId <- authenticate
    post <- requireCheckJsonBody :: Handler EditorPOST
    let project = Project (decodeUtf8 $ encode $ editorPOSTAst post) (editorPOSTName post) userId
    runDB $ P.repsert pid project
    sendResponse ("OK" :: Text)

deleteProjectR :: ProjectId -> Handler ()
deleteProjectR pid = do
    userId <- authenticate
    runDB $ delete $ from $ \project ->
        where_ (project ^. ProjectUserId ==. val userId &&. project ^. ProjectId ==. val pid)
    sendResponse ("OK" :: Text)

getBuildR :: ProjectId -> Handler Value
getBuildR pid = sendResponseStatus notImplemented501 ("Building is not implemented yet." :: Text)

-- Reason behind comment:
-- More work should be done to analyse how each component will be connected.
-- For example: should the backend apropriately adjust the AST so that everything
-- is connected properly?
-- Or should the backend automatically insert the appropriate values in the AST,
-- after it has been generated (what is being done)?
-- My vote is for the former, since it will be easier to implement and doesn't
-- require assuming or analysing how the AST is currently built.
--getBuildR :: ProjectId -> Handler Value
--getBuildR pid = do
--    userId <- authenticate
--    [build] <- runDB $ select $ from $ \project -> do
--        where_ (project ^. ProjectUserId ==. val userId &&. project ^. ProjectId ==. val pid)
--        pure project
--    let ast = eitherDecode $ decodeUtf8 $ projectAst $ entityVal build :: Either String BundleCssLogicUi
--    either (sendResponseStatus status500) (pure . codegenBundle) ast
--  where
--    codegenBundle = do
--        tempPath <- Temp.getCanonicalTemporaryDirectory
--        let file = tempPath </> show pid <> ".zip"
--            css = 
--        Zip.createArchive file (Zip.addEntry Deflate 

-- Reason behind comment:
-- We don't know yet if we'll need it. You're Not Gonna Need It?
--getUserR :: Handler [UserGET]
--getUserR = do
--    users <- runDB $ select $ from $ \(user, project) -> do
--        where_ (user ^. UserId ==. project ^. ProjectUserId)
--        pure (user, project)
--    let users' = fmap (fst . entityVal) users
--        projects = fmap (snd . entityVal) users
--    pure $ map (\(User email _) -> UserGET email $ map snd projects) users'

postUserR :: Handler ()
postUserR = do
    userPOST <- requireCheckJsonBody :: Handler UserPOST
    pwd <- liftIO $ hashPassword 14 (encodeUtf8 $ userPOSTPassword userPOST :: ByteString)
    let user = User (userPOSTEmail userPOST) pwd
    runDB $ insert400_ user
