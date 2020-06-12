module Main where

import Universum

import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Postgresql (withPostgresqlPool)
import Database.Persist.Sql (runMigration, runSqlPool)
import Options.Applicative
import Yesod

import Database
import Foundation

main :: IO ()
main = do
    dbConfig <- execParser opts
    --runStdoutLoggingT $ migrateDB $ connectionString dbConfig
    runStderrLoggingT $ withPostgresqlPool (connectionString dbConfig) (dbConnections dbConfig) \pool -> liftIO do
        runSqlPool (runMigration migrateAll) pool
        warp 3000 $ LowCode pool
  where
    opts = info (parseDBConfig <**> helper)
                (  fullDesc
                <> header "low-code: a backend for low-code-ui"
                <> progDesc "Start the low-code backend" )
