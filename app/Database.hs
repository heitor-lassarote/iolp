{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

module Database where

import Universum

import           Database.Esqueleto
import qualified Database.Persist.Postgresql      as P
import qualified Database.Persist.Postgresql.JSON as P
import qualified Database.Persist.TH              as P
import           Options.Applicative

data DBConfig = DBConfig
    { dbHost        :: Text
    , dbPort        :: Int
    , dbUser        :: Text
    , dbName        :: Text
    , dbPassword    :: Text
    , dbConnections :: Int
    }

parseDBConfig :: Parser DBConfig
parseDBConfig = DBConfig
    <$> strOption
        (  long "host"
        <> short 'h'
        <> metavar "ADDRESS"
        <> help "Host address for the database" )
    <*> option auto
        (  long "port"
        <> short 'p'
        <> metavar "PORT"
        <> help "Port to connect to the database" )
    <*> strOption
        (  long "user"
        <> short 'u'
        <> metavar "USERNAME"
        <> help "The username to connect to the database" )
    <*> strOption
        (  long "dbname"
        <> short 'n'
        <> metavar "DATABASENAME"
        <> help "The name of the database" )
    <*> strOption
        (  long "password"
        <> short 'w'
        <> metavar "PASSWORD"
        <> help "The password to connect to the database" )
    <*> option auto
        (  long "connections"
        <> short 'c'
        <> metavar "CONNECTIONPOOLCOUNT"
        <> help "Number of connections to keep open in the pool" )

connectionString :: DBConfig -> P.ConnectionString
connectionString DBConfig {..} = mconcat $ encodeUtf8 <$>
    [ "host=", dbHost, " "
    , "port=", show dbPort, " "
    , "user=", dbUser, " "
    , "dbname=", dbName, " "
    , "password=", dbPassword
    ]

P.share [P.mkPersist P.sqlSettings, P.mkMigrate "migrateAll"] [P.persistLowerCase|
Project sql=projects
    ast       P.Value
    name      Text
    userId    UserId
User sql=users
    email     Text
    password  ByteString

    UniqueUserEmail email sql=unique_users_email
|]
