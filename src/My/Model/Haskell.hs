{-# LANGUAGE TemplateHaskell #-}

module My.Model.Haskell where


import qualified Data.Text as T

import Data.Aeson.TH
import Data.Time.Calendar

import Database.Persist.TH


type UserAuthId = T.Text


data DashboardAccessType = NoAccess | OwnerAccess
  deriving (Eq, Show, Read)


derivePersistField "DashboardAccessType"


data DueDateOptions = NoDueDate
                    | DueDate Day
                    | DueDateRange Day Day
  deriving (Eq, Ord, Show, Read)


derivePersistField "DueDateOptions"
$(deriveJSON defaultOptions ''DueDateOptions)


data TaskStatus = TaskListed | TaskCompleted | TaskArchived
  deriving (Eq, Ord, Show, Read)


derivePersistField "TaskStatus"
$(deriveJSON defaultOptions ''TaskStatus)
