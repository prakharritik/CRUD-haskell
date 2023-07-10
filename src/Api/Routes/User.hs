{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

module Api.Routes.User where

import Models.User
import Database.Persist.Postgresql (ConnectionString)
import Database.Persist (entityVal)
import Data.Proxy
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Servant.API
import Servant.Server
import Data.Int (Int64)
import Control.Monad.IO.Class (liftIO)
import DB (createUser)


type UserAPI =
    "users" :> ReqBody '[JSON] User :> Post '[JSON] Int64

userAPI :: Proxy UserAPI
userAPI = Proxy :: Proxy UserAPI

createUserHandler :: ConnectionString -> User -> Handler Int64
createUserHandler connString user = liftIO $ createUser connString user

usersServer :: ConnectionString -> Server UserAPI
usersServer connString =
  createUserHandler connString



