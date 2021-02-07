{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module My.Application where


import Data.Maybe (catMaybes)

import Database.Persist.Postgresql
import Yesod
import Yesod.Auth

import My.Foundation
import My.Model
import My.Widgets.Dashboard


mkYesodDispatch "PlanApp" $(parseRoutesFile "config/routes.yesodroutes")


authMaybe :: Handler (Maybe (Entity User))
authMaybe = do
  mbId <- maybeAuthId
  case mbId of
    Just authId -> runDB $ getBy (UniqueUserAuthId authId)
    Nothing -> pure Nothing


authRedirect :: Handler (Entity User)
authRedirect = do
  mbUser <- authMaybe
  case mbUser of
    Just user -> pure user
    Nothing -> redirect $ AuthR LoginR


getHomeRAuthed :: Entity User -> Handler Html
getHomeRAuthed user = do
  dashboards <- runDB $ do
    let userId = entityKey user
    let getE key = fmap (Entity key) <$> get key
    permissions <- selectList [ DashboardAccessUser ==. userId,
                                DashboardAccessAtype ==. OwnerAccess ] []
    catMaybes <$> mapM (getE . dashboardAccessDashboard . entityVal) permissions
  defaultLayout
    [whamlet|
      <ul>
        $forall dashboard <- dashboards
          <li>
            <a href=@{DashboardR $ entityKey dashboard}>
              #{dashboardName $ entityVal dashboard}
      <p>
        <a href=@{CreateDashboardR}>Create dashboard
      <p>
        <a href=@{AuthR LogoutR}>Logout
    |]


getHomeR :: Handler Html
getHomeR = do
  mbAuthId <- maybeAuthId
  case mbAuthId of
    Nothing -> redirect $ AuthR LoginR
    Just authId -> do
      mbUser <- runDB $ getBy (UniqueUserAuthId authId)
      case mbUser of
        Just user -> getHomeRAuthed user
        Nothing -> do
          let user = User authId
          userId <- runDB $ insert user
          getHomeRAuthed $ Entity userId user


createDashboardForm :: Form Dashboard
createDashboardForm = renderDivs $ Dashboard <$> areq textField "Dashboard name: " Nothing


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


authDashboard :: DashboardId -> Handler Dashboard
authDashboard dashboardId = do
  authorize
  dashboardById
    where
      dashboardById :: Handler Dashboard
      dashboardById = do
        mbDashboard <- runDB $ get dashboardId
        case mbDashboard of
          Just e  -> return e
          Nothing -> redirect HomeR

      authorize :: Handler ()
      authorize = do
        userId <- entityKey <$> authRedirect
        accessType <- dashboardAccessType userId dashboardId
        case accessType of
          OwnerAccess -> pure ()
          NoAccess -> redirect HomeR


getDashboardR :: DashboardId -> Handler Html
getDashboardR dashboardId = do 
  dashboard <- authDashboard dashboardId
  let dashboardEntity = Entity dashboardId dashboard
  dashboardWidget dashboardEntity


postCreateTaskR :: DashboardId -> Handler Html
postCreateTaskR dashboardId = do
  _ <- authDashboard dashboardId
  ((result, _), _) <- runFormPost (editTaskForm dashboardId Nothing)
  case result of
    FormSuccess task -> runDB $ insert_ task
    _ -> pure ()
  redirect $ DashboardR dashboardId


postCompleteTaskR :: DashboardId -> TaskId -> Handler Html
postCompleteTaskR dashboardId taskId = do
  _ <- authDashboard dashboardId
  runDB $ update taskId [ TaskStatus =. TaskCompleted ]
  redirect $ DashboardR dashboardId


postArchiveTaskR :: DashboardId -> TaskId -> Handler Html
postArchiveTaskR dashboardId taskId = do
  _ <- authDashboard dashboardId
  runDB $ update taskId [ TaskStatus =. TaskArchived ]
  redirect $ DashboardR dashboardId


postReopenTaskR :: DashboardId -> TaskId -> Handler Html
postReopenTaskR dashboardId taskId = do
  _ <- authDashboard dashboardId
  runDB $ update taskId [ TaskStatus =. TaskListed ]
  redirect $ DashboardR dashboardId


postEditTaskR :: DashboardId -> TaskId -> Handler Html
postEditTaskR dashboardId taskId = do
  _ <- authDashboard dashboardId
  task <- runDB $ get404 taskId
  ((result, _), _) <- runFormPost (editTaskForm dashboardId (Just $ Entity taskId task))
  case result of
    FormSuccess updatedTask -> runDB $ do
      update taskId [ TaskName =. taskName updatedTask,
                      TaskDue =. taskDue updatedTask,
                      TaskDeadline =. taskDeadline updatedTask,
                      TaskDescription =. taskDescription updatedTask ]
    _ -> pure ()
  redirect $ DashboardR dashboardId
