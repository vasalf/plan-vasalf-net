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


import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import Data.Time.Calendar (Day)


import Yesod


import My.Model
import My.Foundation


dashboardWidget :: Entity Dashboard -> Handler Html
dashboardWidget dashboard@(Entity dashboardId _) = do
  tasks <- runDB $ selectList [ TaskDashboard ==. dashboardId, TaskStatus !=. TaskArchived ] []
  let stasks = sortTasks tasks
  widgets <- mapM (modalFormToWidget dashboardId) $ Nothing : map Just stasks
  defaultLayout $ widget dashboard stasks widgets


sortTasks :: [Entity Task] -> [Entity Task]
sortTasks = sortBy cmp
  where
    cmpStatus a b | a == b = EQ
    cmpStatus TaskListed _ = LT
    cmpStatus _ TaskListed = GT
    cmpStatus TaskCompleted _ = LT
    cmpStatus _ TaskCompleted = GT
    cmpStatus _ _ = EQ

    cmpDue a b
      | a == b = EQ
      | NoDueDate <- b = LT
      | NoDueDate <- a = GT
      | DueDate da <- a, DueDate db <- b = compare da db
      | DueDate da <- a, DueDateRange db _ <- b = compare da db
      | DueDateRange da _ <- a, DueDate db <- b = compare da db
      | DueDateRange da _ <- a, DueDateRange db _ <- b = compare da db

    cmpDeadline a b
      | a == b = EQ
      | Nothing <- a = GT
      | Nothing <- b = LT
      | Just da <- a, Just db <- b = compare da db

    cmp (Entity _ lhs) (Entity _ rhs)
      | taskStatus lhs /= taskStatus rhs = cmpStatus (taskStatus lhs) (taskStatus rhs)
      | TaskListed <- taskStatus lhs,
        NoDueDate <- taskDue lhs,
        NoDueDate <- taskDue rhs = cmpDeadline (taskDeadline lhs) (taskDeadline rhs)
      | TaskListed <- taskStatus lhs = cmpDue (taskDue lhs) (taskDue rhs)
      | otherwise = cmpDue (taskDue rhs) (taskDue lhs)


showDue :: DueDateOptions -> T.Text
showDue NoDueDate                = ""
showDue (DueDate day)            = T.pack $ show day
showDue (DueDateRange start end) = T.pack (show start) <> " — " <> T.pack (show end)


showDeadline :: Maybe Day -> T.Text
showDeadline Nothing  = ""
showDeadline (Just d) = T.pack $ show d


taskRow :: DashboardId -> Entity Task -> Widget
taskRow dashboardId et@(Entity taskId task) = case taskStatus task of
  TaskListed ->
    [whamlet|
      <tr class="task-row" data-plan-task=#{textTaskId (Just et)}>
        <td>#{taskName task}
        <td>#{showDue $ taskDue task}
        <td>#{showDeadline $ taskDeadline task}
        <td class="task-btn">
          <form method=post action=@{CompleteTaskR dashboardId taskId}>
            <button class="btn btn-outline-success">Complete
        <td class="task-btn">
          <form method=post action=@{ArchiveTaskR dashboardId taskId}>
            <button class="btn btn-outline-danger">Archive
    |]
  _ ->
    [whamlet|
      <tr class="text-secondary task-row" data-plan-task=#{textTaskId (Just et)}>
        <td><s>#{taskName task}</s>
        <td><s>#{showDue $ taskDue task}
        <td><s>#{showDeadline $ taskDeadline task}
        <td class="task-btn">
          <form method=post action=@{ReopenTaskR dashboardId taskId}>
            <button class="btn btn-outline-primary">Reopen
        <td class="task-btn">
          <form method=post action=@{ArchiveTaskR dashboardId taskId}>
            <button class="btn btn-outline-danger">Archive
    |]


widget :: Entity Dashboard -> [Entity Task] -> [Widget] -> Widget
widget (Entity dashboardId dashboard) tasks modals = do
  setTitle $ toHtml $ dashboardName dashboard
  -- Table
  [whamlet|
    <div class="container">
      <h1>#{dashboardName dashboard}
      <table class="table table-hover">
        <colgroup>
          <col class="col-md-6">
          <col class="col-md-3">
          <col class="col-md-1">
          <col class="col-md-1">
          <col class="col-md-1">
        <thead>
          <tr>
            <th>Task
            <th>Due
            <th>Deadline
            <th>
            <th>
        <tbody>
          $forall task <- tasks
            ^{taskRow dashboardId task}
      <button #add-task class="btn btn-outline-primary">Add task
  |]
  -- Modals
  [whamlet|
    $forall modal <- modals
      ^{modal}
  |]
  -- CSS
  toWidgetHead [cassius|
    .pseudolink
      cursor: pointer
  |]
  -- JS
  toWidgetBody [julius|
    $(function() {
      $("#add-task").click(function() {
        $(".plan-edit-task").filter(function() {
          return $(this).data("plan-task") == #{textTaskId Nothing};
        }).modal("show");
      });
      $(".task-row").click(function() {
        var textId = $(this).data("plan-task");
        $(".plan-edit-task").filter(function() {
          return $(this).data("plan-task") == textId;
        }).modal("show");
      });
      $(".task-btn button").click(function(event) {
        event.stopPropagation();
      });
      $(".to-date-range").click(function() {
        var modalBody = $(this).closest(".modal-body");
        modalBody.find(".date-range").find("input").val(
          modalBody.find(".single-date").find("input").val()
        );
        modalBody.find(".single-date").find("input").val("");
        modalBody.find(".single-date").hide();
        modalBody.find(".date-range").show();
      });
      $(".to-single-date").click(function() {
        var modalBody = $(this).closest(".modal-body");
        modalBody.find(".single-date").find("input").val(
          modalBody.find(".date-range").find("input").first().val()
        );
        modalBody.find(".date-range").find("input").val("");
        modalBody.find(".date-range").hide();
        modalBody.find(".single-date").show();
      });
    });
  |]


makeBootstrapSettings :: [(T.Text, T.Text)] -> FieldSettings PlanApp
makeBootstrapSettings attrs =
  FieldSettings {
    fsLabel = "",
    fsTooltip = Nothing,
    fsId = Nothing,
    fsName = Nothing,
    fsAttrs = ("class", "form-control"):attrs
  }


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


modalTitle :: Maybe Task -> T.Text
modalTitle (Just task) = taskName task
modalTitle Nothing = "New task"


textTaskId :: Maybe (Entity Task) -> T.Text
textTaskId Nothing = "new-task"
textTaskId (Just et) = "task-" <> T.pack (show (entityKey et))


modalFormToWidget :: DashboardId -> Maybe (Entity Task) -> Handler Widget
modalFormToWidget dashboardId et = do
  (formWidget, enctype) <- generateFormPost $ editTaskForm dashboardId et
  pure $ case et of
      Nothing -> 
        [whamlet|
          <form method=post action=@{CreateTaskR dashboardId} enctype=#{enctype}>
            ^{formWidget}
        |]
      Just (Entity taskId _) ->
        [whamlet|
          <form method=post action=@{EditTaskR dashboardId taskId} enctype=#{enctype}>
            ^{formWidget}
        |]


editTaskForm :: DashboardId -> Maybe (Entity Task) -> Form Task
editTaskForm dashboardId task csrf = do
  let bsSettings = makeBootstrapSettings []
  (nameRes, nameView) <- mreq textField bsSettings (taskName . entityVal <$> task)
  (dueDayRes, dueDayView) <- mopt dayField bsSettings (defaultDueDate . entityVal <$> task)
  (dueStartRes, dueStartView) <- mopt dayField bsSettings (defaultDueStart . entityVal <$> task)
  (dueEndRes, dueEndView) <- mopt dayField bsSettings (defaultDueEnd . entityVal <$> task)
  (deadlineRes, deadlineView) <- mopt dayField bsSettings (taskDeadline . entityVal <$> task)
  let taSettings = makeBootstrapSettings [("rows", "7")]
  (descRes, descView) <- mopt textareaField taSettings (defaultDesc . entityVal <$> task)

  let (displaySingle, displayRange) :: (String, String) = case taskDue . entityVal <$> task of
        Just (DueDateRange _ _) -> ("display:none", "display:auto")
        _                       -> ("display:auto", "display:none")

  let res = createTask
        <$> nameRes
        <*> dueDayRes
        <*> dueStartRes
        <*> dueEndRes
        <*> deadlineRes
        <*> (unTextarea . fromMaybe "" <$> descRes)

  let formWidget = [whamlet|
        ^{csrf}
        <div class="modal fade plan-edit-task" data-plan-task=#{textTaskId task}>
          <div class="modal-dialog">
            <div class="modal-content">
              <div class="modal-header">
                <h5 class="modal-title">#{modalTitle $ entityVal <$> task}
                <button class="btn-close" data-bs-dismiss="modal">
              <div class="modal-body">
                  <div>
                    <label for=#{fvId nameView} class="form-label">Task name:
                    ^{fvInput nameView}
                  <div class="single-date" style=#{displaySingle}>
                    <label for=#{fvId dueDayView} class="form-label">Due date:
                    ^{fvInput dueDayView}
                    <p class="text-primary pseudolink to-date-range"><u>Enter date range</u>
                  <div class="date-range" style=#{displayRange}>
                    <div>
                      <label for=#{fvId dueStartView} class="form-label">Due from:
                      ^{fvInput dueStartView}
                    <div>
                      <label for=#{fvId dueEndView} class="form-label">Due to:
                      ^{fvInput dueEndView}
                    <p class="text-primary pseudolink to-single-date"><u>Enter single date</u>
                  <div>
                    <label for=#{fvId deadlineView} class="form-label">Deadline:
                    ^{fvInput deadlineView}
                  <div>
                    <label for=#{fvId descView} class="form-label">Description:
                    ^{fvInput descView}
              <div class="modal-footer">
                <button class="btn btn-outline-primary">Save
                <button class="btn btn-outline-secondary" type="button" data-bs-dismiss="modal">Close
    |]

  pure (res, formWidget)
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
