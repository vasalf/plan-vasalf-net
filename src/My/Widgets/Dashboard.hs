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

module My.Widgets.Dashboard (
  dashboardWidget, editTaskForm
) where


import Control.Monad  (forM)
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import Data.Time.Calendar (Day)


import Yesod


import My.Model
import My.Foundation


data TaskWithEditForm = TaskWithEditForm {
  twefTask :: Entity Task,
  twefForm :: (Widget, Enctype)
}


dashboardWidget :: Entity Dashboard -> Handler Html
dashboardWidget (Entity dashboardId dashboard) = do
  tasks <- dashboardTasks dashboardId
  twefs <- forM tasks $ \task -> do
    form <- generateFormPost $ editTaskForm dashboardId $ Just task
    return $ TaskWithEditForm task form
  newTaskForm <- generateFormPost $ editTaskForm dashboardId Nothing
  defaultLayout $ do
    addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.5.1/jquery.min.js"
    [whamlet|
      <h1>#{dashboardName dashboard}
      ^{tasksWidget dashboardId twefs newTaskForm}
    |]


dashboardTasks :: DashboardId -> Handler [Entity Task]
dashboardTasks dashboardId = 
  runDB $ selectList [TaskDashboard ==. dashboardId, TaskStatus !=. TaskArchived ] []


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
      $(".task-button").click(function(event) {
        event.stopPropagation();
      });
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
        <button>Save
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
        <div class="task-cell">
          <form method=post class="task-button" action=@{CompleteTaskR dashboardId taskId}>
            <button>Complete
      $else
        <div class="task-cell task-reopen">
          <form method=post class="task-button" action=@{ReopenTaskR dashboardId taskId}>
            <button>Reopen
      <div class="task-cell task-archive">
        <form method=post class="task-button" action=@{ArchiveTaskR dashboardId taskId}>
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
