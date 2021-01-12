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


import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import Data.Time.Calendar (Day)

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
/dashboard/#DashboardId/create-task  CreateTaskR POST
/dashboard/#DashboardId/complete-task/#TaskId CompleteTaskR POST
/dashboard/#DashboardId/archive-task/#TaskId ArchiveTaskR POST
/dashboard/#DashboardId/reopen-task/#TaskId ReopenTaskR POST
/api/tasks/#DashboardId ApiTasksR GET
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
  authenticate
  dashboardById
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


getDashboardR :: DashboardId -> Handler Html
getDashboardR dashboardId = do
  dashboard <- authDashboard dashboardId
  (widget, enctype) <- generateFormPost (createTaskForm dashboardId)
  tasks <- dashboardTasks dashboardId
  defaultLayout
    [whamlet|
      <p>#{show dashboard}
      ^{tasksWidget dashboardId tasks}
      <form method=post action=@{CreateTaskR dashboardId} enctype=#{enctype}>
        ^{widget}
        <button>Submit
    |]


dashboardTasks :: DashboardId -> Handler [Entity Task]
dashboardTasks dashboardId = 
  runDB $ selectList [TaskDashboard ==. dashboardId, TaskStatus !=. TaskArchived ] []


getApiTasksR :: DashboardId -> Handler Value
getApiTasksR dashboardId = do
  _ <- authDashboard dashboardId
  tasks <- dashboardTasks dashboardId
  return $ object [ "dashboardId" .= dashboardId, "tasks" .= map entityVal tasks ]


createTaskForm :: DashboardId -> Form Task
createTaskForm dashboardId = renderDivs $ do
  createTask
    <$> areq textField "Task name: " Nothing
    <*> aopt dayField "Due start: " Nothing
    <*> aopt dayField "Due end: " Nothing
    <*> aopt dayField "Deadline: " Nothing
    <*> (unTextarea . fromMaybe "" <$> aopt textareaField "Task description: " Nothing)
  where
    createTask :: T.Text -> Maybe Day -> Maybe Day -> Maybe Day -> T.Text -> Task
    createTask name dueStart dueEnd deadline desc
      | Nothing <- dueStart =
          Task dashboardId name NoDueDate deadline desc TaskListed
      | Just due <- dueStart, Nothing <- dueEnd =
          Task dashboardId name (DueDate due) deadline desc TaskListed
      | Just start <- dueStart, Just end <- dueEnd =
          Task dashboardId name (DueDateRange start end) deadline desc TaskListed


taskRowWidget :: DashboardId -> Entity Task -> Widget
taskRowWidget dashboardId taskEntity = do
  let task = entityVal taskEntity
  let taskId = entityKey taskEntity
  toWidget [whamlet|
    <div class="task-row">
      <div class="task-cell task-name">#{taskName task}
      <div class="task-cell task-due">#{showDue (taskDue task)}
      <div class="task-cell task-deadline">#{show $ taskDeadline task}
      <div class="task-cell task-status"> #{show $ taskStatus task}
      $if taskStatus task == TaskListed
        <div class="task-cell task-complete">
          <form method=post action=@{CompleteTaskR dashboardId taskId}>
            <button>Complete
      $else
        <div class="task-cell task-reopen">
          <form method=post action=@{ReopenTaskR dashboardId taskId}>
            <button>Reopen
      <div class="task-cell task-archive">
        <form method=post action=@{ArchiveTaskR dashboardId taskId}>
          <button>Archive
  |]
    where
      showDue NoDueDate = ""
      showDue (DueDate date) = show date
      showDue (DueDateRange start end) = show start <> "—" <> show end


tasksWidget :: DashboardId -> [Entity Task] -> Widget
tasksWidget dashboardId tasks = do
  toWidgetHead [cassius|
    #tasks:
      display: table
    .task-row
      display: table-row
    .task-header
      font-weight: bold
    .task-cell
      display: table-cell
      padding: 5px
      text-align: left
  |]
  toWidget [whamlet|
    <div #tasks>
      <div class="task-row">
        <div class="task-cell task-header">Task
        <div class="task-cell task-header">Due
        <div class="task-cell task-header">Deadline
      $forall task <- tasks
        ^{taskRowWidget dashboardId task}
  |]


postCreateTaskR :: DashboardId -> Handler Html
postCreateTaskR dashboardId = do
  _ <- authDashboard dashboardId
  ((result, _), _) <- runFormPost (createTaskForm dashboardId)
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
