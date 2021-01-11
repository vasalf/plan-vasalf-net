{-# LANGUAGE OverloadedStrings #-}

module Main where

import My.Plan

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Data.ByteString (ByteString)

import Database.Persist.Postgresql 
import Yesod (warp)


psqlConnStr :: ByteString
psqlConnStr = "host=localhost "
  <> "dbname=plan "
  <> "user=test "
  <> "password=test "
  <> "port=5432"


main :: IO ()
main =
  runStderrLoggingT $
  withPostgresqlPool psqlConnStr 10 $
  \pool -> liftIO $ do
    flip runSqlPersistMPool pool $ do
      runMigration migrateAll
    warp 3000 $ PlanApp pool
