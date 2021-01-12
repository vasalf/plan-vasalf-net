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
{-# LANGUAGE ViewPatterns #-}

module My.Plan where


import Database.Persist.Postgresql
import Yesod

import My.Model


newtype PlanApp = PlanApp {
  connectionPool :: ConnectionPool
}


mkYesod "PlanApp" [parseRoutes|
/ HomeR GET
/create-dashboard CreateDashboardR GET POST
/dashboard/#DashboardId DashboardR GET
|]


instance Yesod PlanApp


instance YesodPersist PlanApp where
  type YesodPersistBackend PlanApp = SqlBackend
  runDB action = (connectionPool <$>  getYesod) >>= runSqlPool action


instance RenderMessage PlanApp FormMessage where
  renderMessage _ _ = defaultFormMessage


type Form a = Html -> MForm Handler (FormResult a, Widget)


-- TODO: Authentication
authMaybe :: Handler (Maybe (Entity User))
authMaybe = runDB $ getBy $ UniqueUserAuthId "test"


authRedirect :: Handler (Entity User)
authRedirect = do
  mbUser <- authMaybe
  case mbUser of
    Just user -> pure user
    Nothing -> redirect HomeR


getHomeR :: Handler Html
getHomeR = do
  mbUser <- authMaybe
  defaultLayout
    [whamlet|
      <p>Hi, #{show mbUser}!
      <p><a href=@{CreateDashboardR}>Create dashboard
    |]


createDashboardForm :: Form Dashboard
createDashboardForm = renderDivs $ Dashboard <$> areq textField "Dashboard Name: " Nothing


getCreateDashboardR :: Handler Html
getCreateDashboardR = do
  _ <- authRedirect
  (widget, enctype) <- generateFormPost createDashboardForm
  defaultLayout
    [whamlet|
      <form method=post action=@{CreateDashboardR} enctype=#{enctype}>
        ^{widget}
        <button>Submit
    |]


postCreateDashboardR :: Handler Html
postCreateDashboardR = do
  ((result, _), _) <- runFormPost createDashboardForm
  case result of
    FormSuccess dashboard -> do
      userId <- entityKey <$> authRedirect
      dashboardId <- runDB $ do
        dashboardId <- insert dashboard
        insert_ $ DashboardAccess userId dashboardId OwnerAccess
        return dashboardId
      redirect $ DashboardR dashboardId
    _ -> getCreateDashboardR


dashboardAccessType :: UserId -> DashboardId -> Handler DashboardAccessType
dashboardAccessType userId dashboardId = runDB $ do
  mbType <- getBy $ UniqueDashboardAuthId userId dashboardId
  return $ maybe NoAccess (dashboardAccessAtype . entityVal) mbType


getDashboardR :: DashboardId -> Handler Html
getDashboardR dashboardId = do
  authenticate
  dashboard <- dashboardById
  defaultLayout
    [whamlet|
      <p>#{show dashboard}
    |]
  where
    dashboardById :: Handler Dashboard
    dashboardById = do
      mbDashboard <- runDB $ get dashboardId
      case mbDashboard of
        Just e  -> return e
        Nothing -> redirect HomeR

    authenticate :: Handler ()
    authenticate = do
      userId <- entityKey <$> authRedirect
      accessType <- dashboardAccessType userId dashboardId
      case accessType of
        OwnerAccess -> pure ()
        NoAccess -> redirect HomeR
