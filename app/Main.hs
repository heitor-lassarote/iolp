module Main where

import Universum

import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Postgresql (withPostgresqlPool)
import Database.Persist.Sql (runMigration, runSqlPool)
import Options.Applicative
import Yesod

import Database
import Foundation

data ServerConfig = ServerConfig
    { dbConfig          :: DBConfig
    , serverConnections :: Int
    , serverPort        :: Int
    }

parseServerConfig :: Parser ServerConfig
parseServerConfig = ServerConfig
    <$> parseDBConfig
    <*> option auto
        (  long "connections"
        <> short 'c'
        <> metavar "CONNECTION_POOL_COUNT"
        <> help "Number of connections to keep open in the pool" )
    <*> option auto
        (  long "port"
        <> short 'p'
        <> metavar "PORT"
        <> help "Port to connect to server" )

main :: IO ()
main = do
    config <- execParser opts
    runStderrLoggingT $ withPostgresqlPool (connectionString $ dbConfig config) (serverConnections config) \pool -> liftIO do
        runSqlPool (runMigration migrateAll) pool
        warp (serverPort config) $ LowCode pool
  where
    opts = info (parseServerConfig <**> helper)
                (  fullDesc
                <> header "low-code: a backend for low-code-ui"
                <> progDesc "Start the low-code backend" )
