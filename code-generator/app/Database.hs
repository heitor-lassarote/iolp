{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

module Database where

import Universum

import           Database.Esqueleto
import qualified Database.Persist.Postgresql      as P
import qualified Database.Persist.Postgresql.JSON as P
import qualified Database.Persist.TH              as P

import Config

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
