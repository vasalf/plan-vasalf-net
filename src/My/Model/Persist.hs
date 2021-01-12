{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module My.Model.Persist where


import qualified Data.Text as T

import Data.Aeson.TH
import Database.Persist.Postgresql
import Data.Time.Calendar
import Yesod

import My.Model.Haskell


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  authId UserAuthId
  deriving Eq
  deriving Ord
  deriving Show
  UniqueUserAuthId authId

Dashboard
  name T.Text
  deriving Eq
  deriving Ord
  deriving Show

DashboardAccess
  user UserId
  dashboard DashboardId
  atype DashboardAccessType
  deriving Eq
  deriving Show
  UniqueDashboardAuthId user dashboard

Task
  dashboard DashboardId
  name T.Text
  due DueDateOptions default="\'NoDueDate\'"
  deadline Day Maybe
  description T.Text
  deriving Eq
  deriving Ord
  deriving Show
|]


$(deriveJSON defaultOptions 'Task)
