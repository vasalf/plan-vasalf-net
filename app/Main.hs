{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Config
import My.Model
import My.Plan

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Data.Maybe (fromMaybe)

import Data.Yaml (decodeFileThrow)
import Database.Persist.Postgresql
import Yesod (warp)


main :: IO ()
main =
  runStderrLoggingT $ do
    runCfg :: RunConfig <- decodeFileThrow "config/run.yaml"
    secretCfg :: SecretConfig <- decodeFileThrow "config/secret.yaml"
    let psqlConnStr = makePsqlConnStr (postgresConfig secretCfg)
    withPostgresqlPool psqlConnStr (postgresConnections runCfg) $
      \pool -> liftIO $ do
        flip runSqlPersistMPool pool $ runMigration migrateAll
        warp (runPort runCfg) $ PlanApp pool (googleOAuthSecrets secretCfg)
