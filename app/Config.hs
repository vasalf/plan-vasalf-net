{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}


module Config where


import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8)

import qualified Data.Text as T


data RunConfig = RunConfig {
  runPort :: Int,
  postgresConnections :: Int
}


$(deriveJSON defaultOptions 'RunConfig)


data PostgresConfig = PostgresConfig {
  postgresHost :: T.Text,
  postgresDbName :: T.Text,
  postgresUser :: T.Text,
  postgresPassword :: T.Text,
  postgresPort :: Int
}


makePsqlConnStr :: PostgresConfig -> ByteString
makePsqlConnStr cfg = encodeUtf8 $
  "host=" <> postgresHost cfg <> " "
  <> "dbname=" <> postgresDbName cfg <> " "
  <> "user=" <> postgresUser cfg <> " "
  <> "password=" <> postgresPassword cfg <> " "
  <> "port=" <> T.pack (show $ postgresPort cfg)


$(deriveJSON defaultOptions 'PostgresConfig)


newtype SecretConfig = SecretConfig {
  postgresConfig :: PostgresConfig
}


$(deriveJSON defaultOptions 'SecretConfig)
