module Main where

import Universum

import Control.Monad.Logger (liftLoc, runStderrLoggingT)
import Data.Default.Class
import Database.Persist.Postgresql (ConnectionPool, withPostgresqlPool)
import Database.Persist.Sql (runMigration, runSqlPool)
import Language.Haskell.TH.Syntax (qLocation)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp
    ( Settings
    , defaultSettings
    , defaultShouldDisplayException
    , runSettings
    , setOnException
    , setPort
    )
import Network.Wai.Middleware.Cors
    ( CorsResourcePolicy (..)
    , cors
    , simpleCorsResourcePolicy
    , simpleCors
    )
import Network.Wai.Middleware.RequestLogger
    ( Destination (Logger)
    , IPAddrSource (..)
    , OutputFormat (..)
    , destination
    , mkRequestLogger
    , outputFormat
    )
import Options.Applicative
import System.Log.FastLogger (defaultBufSize, newStdoutLoggerSet, toLogStr)
import Yesod
import Yesod.Core.Types (loggerSet)
import Yesod.Default.Config2 (makeYesodLogger)

import Config
import Database
import Foundation

allowCors :: Middleware
allowCors = cors (const $ Just appCorsResourcePolicy)

appCorsResourcePolicy :: CorsResourcePolicy
appCorsResourcePolicy =
    simpleCorsResourcePolicy
        { corsMethods = ["OPTIONS", "GET", "PUT", "POST"]
        , corsRequestHeaders = ["Authorization", "Content-Type"]
        }

mkFoundation :: ServerConfig -> ConnectionPool -> IO LowCode
mkFoundation serverConfig pool = do
    runSqlPool (runMigration migrateAll) pool
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    pure $ LowCode appLogger pool serverConfig

mkApplication :: LowCode -> IO Application
mkApplication foundation = do
    logWare <- makeLogWare foundation
    -- Create the WAI application and apply middlewares
    appPlain <- toWaiAppPlain foundation
    pure $ logWare $ defaultMiddlewaresNoLogging $ simpleCors appPlain

makeLogWare :: LowCode -> IO Middleware
makeLogWare foundation =
    mkRequestLogger def
        { outputFormat =
            if serverDetailedRequestLogging $ serverConfig foundation
                then Detailed True
                else Apache
                        (if serverIpFromHeader $ serverConfig foundation
                            then FromFallback
                            else FromSocket)
        , destination = Logger $ loggerSet $ appLogger foundation
        }

-- | Warp settings for the given foundation value.
warpSettings :: LowCode -> Settings
warpSettings foundation =
      setPort (serverPort $ serverConfig foundation)
    $ setOnException (\_req e ->
            when (defaultShouldDisplayException e) $ messageLoggerSource
                foundation
                (appLogger foundation)
                $(qLocation >>= liftLoc)
                "yesod"
                LevelError
                (toLogStr $ "Exception from Warp: " ++ show e))
      defaultSettings

main :: IO ()
main = do
    config <- execParser opts
    runStderrLoggingT $ withPostgresqlPool (connectionString $ dbConfig config) (serverConnections config) \pool -> liftIO do
        foundation <- mkFoundation config pool
        app <- mkApplication foundation
        runSettings (warpSettings foundation) app
  where
    opts = info (parseServerConfig <**> helper)
                (  fullDesc
                <> header "low-code: a backend for low-code-ui"
                <> progDesc "Start the low-code backend" )
