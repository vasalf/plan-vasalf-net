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


instance Yesod PlanApp where
  approot = ApprootMaster hostname


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
