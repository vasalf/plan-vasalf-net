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


import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import Data.Time.Calendar (Day)
import Text.Julius (RawJS(..))

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
  defaultLayout $ do
    addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.5.1/jquery.min.js"
    toWidgetHead [cassius|
      #new-task-popup
        display: none
        position: absolute
        top: 10%
        left: 10%
        border: 1px solid black
        background-color: white
        padding: 5%
        z-index: 10
    |]
    [whamlet|
      <p>#{show dashboard}
      ^{tasksWidget dashboardId tasks}
      <div #new-task-popup>
        <form method=post action=@{CreateTaskR dashboardId} enctype=#{enctype}>
          ^{widget}
          <button>Submit
          <button #new-task-popup-close>Close
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
createTaskForm dashboardId csrf = do
  (nameRes, nameView) <- mreq textField "" Nothing
  (dueDayRes, dueDayView) <- mopt dayField "" Nothing
  (dueStartRes, dueStartView) <- mopt dayField "" Nothing
  (dueEndRes, dueEndView) <- mopt dayField "" Nothing
  (deadlineRes, deadlineView) <- mopt dayField "" Nothing
  (descRes, descView) <- mopt textareaField "" Nothing
  let res = createTask
        <$> nameRes
        <*> dueDayRes
        <*> dueStartRes
        <*> dueEndRes
        <*> deadlineRes
        <*> (unTextarea . fromMaybe "" <$> descRes)
  let widget = do
        toWidget [cassius|
          #new-task-form
            display: table
          .new-task-form-row
            display: table-row
          .new-task-form-cell
            display: table-cell
            padding: 5px
          #new-task-due-date-range
            display: none
        |]
        toWidget [julius|
          $(function(){
            $("#switch-to-date-range").click(function(event){
              $("#" + "#{rawJS $ fvId dueDayView}").val("");
              $("#new-task-due-date-range").css("display", "table-row");
              $("#new-task-due-day").hide();
              event.preventDefault();
            });
            $("#switch-to-single-date").click(function(event){
              $("#" + "#{rawJS $ fvId dueStartView}").val("");
              $("#" + "#{rawJS $ fvId dueEndView}").val("");
              $("#new-task-due-date-range").hide();
              $("#new-task-due-day").css("display", "table-row");
              event.preventDefault();
            });
          });
        |]
        [whamlet|
          #{csrf}
          <div #new-task-form>
            <div class="new-task-form-row">
              <div class="new-task-form-cell">
                Task name:
              <div class="new-task-form-cell">
                ^{fvInput nameView}
            <div class="new-task-form-row" #new-task-due-day>
              <div class="new-task-form-cell">
                Due:
              <div class="new-task-form-cell">
                <span>
                  ^{fvInput dueDayView}
                <a href="#" class="interactive-link" #switch-to-date-range>
                  Enter date range
            <div class="new-task-form-row" #new-task-due-date-range>
              <div class="new-task-form-cell">
                Due:
              <div class="new-task-form-cell">
                <span>
                  ^{fvInput dueStartView} — ^{fvInput dueEndView}
                <a href="#" class="interactive-link" #switch-to-single-date>
                  Enter single date
            <div class="new-task-form-row">
              <div class="new-task-form-cell">
                Deadline:
              <div class="new-task-form-cell">
                ^{fvInput deadlineView}
            <div class="new-task-form-row">
              <div class="new-task-form-cell">
                Description:
              <div class="new-task-form-cell">
                ^{fvInput descView}
        |]
  return (res, widget)
    where
      createTask :: T.Text -> Maybe Day -> Maybe Day -> Maybe Day -> Maybe Day -> T.Text -> Task
      createTask name dueDay dueStart dueEnd deadline desc
        | Just due <- dueDay =
            Task dashboardId name (DueDate due) deadline desc TaskListed
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
      <div class="task-cell task-deadline">#{showDeadline $ taskDeadline task}
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

      showDeadline = maybe "" show


tasksWidget :: DashboardId -> [Entity Task] -> Widget
tasksWidget dashboardId tasks = do
  let sortedTasks = sortBy cmp tasks
  toWidgetHead [cassius|
    #tasks
      display: table
    .task-row
      display: table-row
    .task-row:hover
      background-color: grey
    .task-header
      display: table-row
      font-weight: bold
    .task-cell
      display: table-cell
      padding: 5px
      text-align: left
  |]
  toWidget [julius|
    $(function(){
      $("#new-task-row").click(function(){
        $("#new-task-popup").show();
      });
      $("#new-task-popup-close").click(function(){
        $("#new-task-popup").hide();
      });
    });
  |]
  toWidget [whamlet|
    <div #tasks>
      <div class="task-header">
        <div class="task-cell">Task
        <div class="task-cell">Due
        <div class="task-cell">Deadline
      $forall task <- sortedTasks
        ^{taskRowWidget dashboardId task}
      <div class="task-row" #new-task-row>
        <div class="task-cell">New task
        <div class="task-cell">
        <div class="task-cell">
        <div class="task-cell">
        <div class="task-cell">
        <div class="task-cell">
  |]
    where
      cmpCmpl x y
        | x == y = EQ
        | x == TaskListed = LT
        | otherwise = GT

      cmpMaybeInv x y
        | Nothing <- x, Nothing <- y = EQ
        | Nothing <- y = LT
        | Nothing <- x = GT
        | Just xv <- x, Just yv <- y = compare xv yv

      mbStartDate NoDueDate = Nothing
      mbStartDate (DueDate s) = Just s
      mbStartDate (DueDateRange s _) = Just s

      dueTf = mbStartDate . taskDue . entityVal

      cmp task1 task2
        | taskStatus (entityVal task1) /= taskStatus (entityVal task2) =
            cmpCmpl (taskStatus $ entityVal task1) (taskStatus $ entityVal task2)
        | taskDue (entityVal task1) /= taskDue (entityVal task2) =
            cmpMaybeInv (dueTf task1) (dueTf task2)
        | taskDeadline (entityVal task1) /= taskDeadline (entityVal task2) =
            cmpMaybeInv (taskDeadline $ entityVal task1) (taskDeadline $ entityVal task2)
        | otherwise =
            compare (entityKey task1) (entityKey task2)


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
