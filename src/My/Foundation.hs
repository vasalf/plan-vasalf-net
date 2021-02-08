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

module My.Foundation where

import qualified Data.Text as T

import Data.Aeson.TH (deriveJSON, defaultOptions)

import Database.Persist.Postgresql
import Yesod
import Yesod.Auth
import Yesod.Auth.OAuth2.Google

import My.Model


data GoogleOAuthTokens = GoogleOAuthTokens {
  googleClientId :: T.Text,
  googleClientSecret :: T.Text
}


$(deriveJSON defaultOptions 'GoogleOAuthTokens)


data PlanApp = PlanApp {
  connectionPool :: ConnectionPool,
  hostname :: T.Text,
  googleSecrets :: GoogleOAuthTokens
}


mkYesodData "PlanApp" $(parseRoutesFile "config/routes.yesodroutes")


yesodLayout :: Widget -> Handler Html
yesodLayout widget = do
  pageContent <- widgetToPageContent widget
  withUrlRenderer
    [hamlet|
      $doctype 5
      <html>
        <head>
          <title>#{pageTitle pageContent}
          <meta charset=utf-8>
          <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.0.0-beta1/dist/css/bootstrap.min.css" rel="stylesheet" integrity="sha384-giJF6kkoqNQ00vy+HMDP7azOuL0xtbfIcaT9wjKHr8RbDVddVHyTfAAsrekwKmP1" crossorigin="anonymous">
          <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.0.0-beta1/dist/js/bootstrap.bundle.min.js" integrity="sha384-ygbV9kiqUc6oa4msXn9868pTtWMgiQaeYH7/t7LECLbyPA2x65Kgf80OJFdroafW" crossorigin="anonymous">
          <script src="https://code.jquery.com/jquery-3.5.1.min.js" integrity="sha384-ZvpUoO/+PpLXR1lu4jmpXWu80pZlYUAfxl5NsBMWOEPSjUn/6Z/hRTt8+pR6L4N2" crossorigin="anonymous">
          ^{pageHead pageContent}
        <body>
          ^{pageBody pageContent}
    |]


instance Yesod PlanApp where
  approot = ApprootMaster hostname
  defaultLayout = yesodLayout


instance YesodPersist PlanApp where
  type YesodPersistBackend PlanApp = SqlBackend
  runDB action = (connectionPool <$> getYesod) >>= runSqlPool action


instance YesodAuth PlanApp where
  type AuthId PlanApp = UserAuthId
  authenticate = return . Authenticated . credsIdent

  loginDest _ = HomeR
  logoutDest _ = HomeR

  authPlugins app = [ oauth2Google (googleClientId ts) (googleClientSecret ts) ]
    where
      ts = googleSecrets app

  maybeAuthId = lookupSession "_ID"


instance RenderMessage PlanApp FormMessage where
  renderMessage _ _ = defaultFormMessage


type Form a = Html -> MForm Handler (FormResult a, Widget)
