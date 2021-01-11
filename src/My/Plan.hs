{-# LANGUAGE DeriveAnyClass #-}
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

module My.Plan where


import Database.Persist.Postgresql
import Yesod

import My.Model


newtype PlanApp = PlanApp {
  connectionPool :: ConnectionPool
}


mkYesod "PlanApp" [parseRoutes|
/ HomeR GET
|]


instance Yesod PlanApp


instance YesodPersist PlanApp where
  type YesodPersistBackend PlanApp = SqlBackend
  runDB action = (connectionPool <$>  getYesod) >>= runSqlPool action


getHomeR :: Handler Html
getHomeR = defaultLayout
  [whamlet|
    <p>Hi there!
  |]

