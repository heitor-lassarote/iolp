module Main where

import Universum

import Control.Monad.Logger (liftLoc, runStderrLoggingT)
import Data.Default.Class
import Database.Persist.Postgresql (ConnectionPool, withPostgresqlPool)
import Database.Persist.Sql (runMigration, runSqlPool)
import Language.Haskell.TH.Syntax (qLocation)
import Network.Wai (Middleware, requestHeaders)
import Network.Wai.Handler.Warp
    ( Settings
    , defaultSettings
    , defaultShouldDisplayException
    , setOnException
    , setPort
    )
import Network.Wai.Handler.WarpTLS
    ( OnInsecure (..)
    , onInsecure
    , runTLS
    , tlsSettings
    )
import Network.Wai.Middleware.Cors
    ( CorsResourcePolicy (..)
    , cors
    , simpleHeaders
    , simpleMethods
    , simpleResponseHeaders
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
allowCors = cors withAnyOrigin
  where
    origin = fmap (bimap pure (const True) . swap) . find ((== "Origin") . fst) . requestHeaders

    withAnyOrigin req = Just appCorsResourcePolicy { corsOrigins = origin req }

appCorsResourcePolicy :: CorsResourcePolicy
appCorsResourcePolicy = CorsResourcePolicy
    { corsExposedHeaders = Just simpleResponseHeaders
    , corsIgnoreFailures = True
    , corsMaxAge = Just 60
    , corsMethods = "DELETE" : "PUT" : "OPTIONS" : simpleMethods
    , corsOrigins = Nothing
    , corsRequestHeaders = "Authorization" : "Content-Type" : simpleHeaders
    , corsRequireOrigin = True
    , corsVaryOrigin = True
    }

mkFoundation :: ServerConfig -> ConnectionPool -> IO LowCode
mkFoundation serverConfig pool = do
    runSqlPool (runMigration migrateAll) pool
    appLogger <- makeYesodLogger =<< newStdoutLoggerSet defaultBufSize
    pure $ LowCode appLogger pool serverConfig

mkApplication :: LowCode -> IO Application
mkApplication foundation = do
    logWare <- makeLogWare foundation
    -- Create the WAI application and apply middlewares
    appPlain <- toWaiAppPlain foundation
    pure $ logWare $ defaultMiddlewaresNoLogging $ allowCors appPlain

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
        runTLS (mkTlsSettings config) (warpSettings foundation) app
  where
    opts = info (parseServerConfig <**> helper)
                (  fullDesc
                <> header "low-code: a backend for low-code-ui"
                <> progDesc "Start the low-code backend" )

    mkTlsSettings config =
        let tlsS = tlsSettings (serverCertificate config) (serverKey config)
         in if serverAcceptInsecureHttp config
                then tlsS { onInsecure = AllowInsecure }
                else tlsS
