module Settings.PostgresSettings where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types (Parser)
import Database.PostgreSQL.Simple

readPostgresConnectInfo :: Object -> Parser ConnectInfo
readPostgresConnectInfo o = do
  dbObject <- o .: "postgres-connect-info"
  ConnectInfo <$> dbObject .: "db-host"
              <*> dbObject .: "db-port"
              <*> dbObject .: "db-user"
              <*> dbObject .: "db-pwd"
              <*> dbObject .: "db-name"

