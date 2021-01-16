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

module My.Plan where


import Control.Monad (forM)
import Data.List (sortBy)
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
/dashboard/#DashboardId/edit-task/#TaskId EditTaskR POST
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


data TaskWithEditForm = TaskWithEditForm {
  twefTask :: Entity Task,
  twefForm :: (Widget, Enctype)
}


getDashboardR :: DashboardId -> Handler Html
getDashboardR dashboardId = do
  dashboard <- authDashboard dashboardId
  tasks <- dashboardTasks dashboardId
  twefs <- forM tasks $ \task -> do
    form <- generateFormPost $ editTaskForm dashboardId $ Just task
    return $ TaskWithEditForm task form
  newTaskForm <- generateFormPost $ editTaskForm dashboardId Nothing
  defaultLayout $ do
    addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.5.1/jquery.min.js"
    [whamlet|
      <p>#{show dashboard}
      ^{tasksWidget dashboardId twefs newTaskForm}
    |]


dashboardTasks :: DashboardId -> Handler [Entity Task]
dashboardTasks dashboardId = 
  runDB $ selectList [TaskDashboard ==. dashboardId, TaskStatus !=. TaskArchived ] []


getApiTasksR :: DashboardId -> Handler Value
getApiTasksR dashboardId = do
  _ <- authDashboard dashboardId
  tasks <- dashboardTasks dashboardId
  return $ object [ "dashboardId" .= dashboardId, "tasks" .= map entityVal tasks ]


editTaskForm :: DashboardId -> Maybe (Entity Task) -> Form Task
editTaskForm dashboardId task csrf = do
  (nameRes, nameView) <- mreq textField "" (taskName . entityVal <$> task)
  (dueDayRes, dueDayView) <- mopt dayField "" (defaultDueDate . entityVal <$> task)
  (dueStartRes, dueStartView) <- mopt dayField "" (defaultDueStart . entityVal <$> task)
  (dueEndRes, dueEndView) <- mopt dayField "" (defaultDueEnd . entityVal <$> task)
  (deadlineRes, deadlineView) <- mopt dayField "" (taskDeadline . entityVal <$> task)
  (descRes, descView) <- mopt textareaField "" (defaultDesc . entityVal <$> task)
  let (singleDisplay, rangeDisplay) :: (String, String) =
        case taskDue . entityVal <$> task of
          Just (DueDateRange _ _) -> ("none", "table-row")
          _                       -> ("table-row", "none")
  let res = createTask
        <$> nameRes
        <*> dueDayRes
        <*> dueStartRes
        <*> dueEndRes
        <*> deadlineRes
        <*> (unTextarea . fromMaybe "" <$> descRes)
  let widget = do
        [whamlet|
          #{csrf}
          <div class="edit-task-form">
            <div class="edit-task-form-row">
              <div class="edit-task-form-cell">
                Task name:
              <div class="edit-task-form-cell">
                ^{fvInput nameView}
            <div class="edit-task-form-row edit-task-due-day"
                 style="display:#{singleDisplay}">
              <div class="edit-task-form-cell">
                Due:
              <div class="edit-task-form-cell">
                <span>
                  ^{fvInput dueDayView}
                <a href="#" class="switch-to-date-range">
                  Enter date range
            <div class="edit-task-form-row edit-task-due-date-range"
                 style="display:#{rangeDisplay}">
              <div class="edit-task-form-cell">
                Due:
              <div class="edit-task-form-cell">
                <span>
                  ^{fvInput dueStartView} — ^{fvInput dueEndView}
                <a href="#" class="switch-to-single-day">
                  Enter single date
            <div class="edit-task-form-row">
              <div class="edit-task-form-cell">
                Deadline:
              <div class="edit-task-form-cell">
                ^{fvInput deadlineView}
            <div class="edit-task-form-row">
              <div class="edit-task-form-cell">
                Description:
              <div class="edit-task-form-cell">
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

      defaultDueDate :: Task -> Maybe Day
      defaultDueDate t =
        case taskDue t of
          DueDate day -> Just day
          _           -> Nothing

      defaultDueStart :: Task -> Maybe Day
      defaultDueStart t =
        case taskDue t of
          DueDateRange start _ -> Just start
          _                    -> Nothing

      defaultDueEnd :: Task -> Maybe Day
      defaultDueEnd t =
        case taskDue t of
          DueDateRange _ end -> Just end
          _                  -> Nothing

      defaultDesc :: Task -> Maybe Textarea
      defaultDesc = Just . Textarea . taskDescription


editTaskCommonWidget :: Widget
editTaskCommonWidget = do
  toWidgetHead [cassius|
    .edit-task-popup
      display: none
      position: absolute
      top: 10%
      left: 10%
      border: 1px solid black
      background-color: white
      padding: 5%
      z-index: 10

    .edit-task-form
      display: table

    .edit-task-form-row
      display: table-row

    .edit-task-form-cell
      display: table-cell
      padding: 5px

    .edit-task-due-date-range
      display: none
  |]
  toWidget [julius|
    $(function(){
      $(".task-row").click(function(){
        if ($(".edit-task-popup").filter(":visible").length == 0) {
          $(this).find(".edit-task-popup").show();
        }
      });
      $(".edit-task-popup-close").click(function(event){
        $(this).parent().hide();
        event.stopPropagation();
      });
      $(".switch-to-date-range").click(function(event){
        var form = $(this).closest(".edit-task-form");
        form.find(".edit-task-due-day").hide();
        form.find(".edit-task-due-day").find("input").val("");
        form.find(".edit-task-due-date-range").css("display", "table-row");
        event.preventDefault();
      });
      $(".switch-to-single-day").click(function(event){
        var form = $(this).closest(".edit-task-form");
        form.find(".edit-task-due-date-range").hide();
        form.find(".edit-task-due-date-range").find("input").val("");
        form.find(".edit-task-due-day").css("display", "table-row");
        event.preventDefault();
      });
    });
  |]


editTaskPopupWidget :: DashboardId -> Maybe (Entity Task) -> (Widget, Enctype) -> Widget
editTaskPopupWidget dashboardId mbTask (widget, enctype) = do
  let formEndRoute = case mbTask of
                        Nothing -> CreateTaskR dashboardId
                        Just task -> EditTaskR dashboardId (entityKey task)
  [whamlet|
    <div class="edit-task-popup">
      <form method="post" action=@{formEndRoute} enctype=#{enctype}>
        ^{widget}
        <button>Submit
      <button .edit-task-popup-close>Close
  |]


taskRowWidget :: DashboardId -> Entity Task -> (Widget, Enctype) -> Widget
taskRowWidget dashboardId taskEntity form = do
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
      ^{editTaskPopupWidget dashboardId (Just taskEntity) form}
  |]
    where
      showDue NoDueDate = ""
      showDue (DueDate date) = show date
      showDue (DueDateRange start end) = show start <> "—" <> show end

      showDeadline = maybe "" show


tasksWidget :: DashboardId -> [TaskWithEditForm] -> (Widget, Enctype) -> Widget
tasksWidget dashboardId tasks newTaskForm = do
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
  editTaskCommonWidget
  toWidget [whamlet|
    <div #tasks>
      <div class="task-header">
        <div class="task-cell">Task
        <div class="task-cell">Due
        <div class="task-cell">Deadline
      $forall TaskWithEditForm task form <- sortedTasks
        ^{taskRowWidget dashboardId task form}
      <div class="task-row" #new-task-row>
        <div class="task-cell">New task
        <div class="task-cell">
        <div class="task-cell">
        <div class="task-cell">
        <div class="task-cell">
        <div class="task-cell">
        ^{editTaskPopupWidget dashboardId Nothing newTaskForm}
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

      cmpE task1 task2
        | taskStatus (entityVal task1) /= taskStatus (entityVal task2) =
            cmpCmpl (taskStatus $ entityVal task1) (taskStatus $ entityVal task2)
        | taskDue (entityVal task1) /= taskDue (entityVal task2) =
            cmpMaybeInv (dueTf task1) (dueTf task2)
        | taskDeadline (entityVal task1) /= taskDeadline (entityVal task2) =
            cmpMaybeInv (taskDeadline $ entityVal task1) (taskDeadline $ entityVal task2)
        | otherwise =
            compare (entityKey task1) (entityKey task2)

      cmp task1 task2 = cmpE (twefTask task1) (twefTask task2)


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
