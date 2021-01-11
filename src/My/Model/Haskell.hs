{-# LANGUAGE TemplateHaskell #-}

module My.Model.Haskell where


import qualified Data.Text as T

import Database.Persist.TH


type UserAuthId = T.Text


data DashboardAccessType = NoAccess | OwnerAccess
  deriving (Eq, Show, Read)


derivePersistField "DashboardAccessType"
