{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

module Api.Routes.User where

import Models.User
    ( LoginRequest(loginPassword, loginEmail),
      User(userEmail, userPassword) )
import Database.Persist.Postgresql (ConnectionString, Entity (entityKey), fromSqlKey)
import Database.Persist (entityVal, entityVal)

import Data.Proxy ( Proxy(..) )
import Data.Maybe (fromMaybe)
import Servant.API
    ( type (:<|>)(..), JSON, ReqBody, type (:>), Post )
import Servant.Server
    ( err401, Server, Handler, ServerError(errBody) )
import Data.Text.Encoding ( encodeUtf8, decodeLatin1 )
import Control.Monad.IO.Class (liftIO)
import DB (createUser, fetchUserByEmail)
import Crypto.BCrypt
    ( hashPasswordUsingPolicy,
      slowerBcryptHashingPolicy,
      validatePassword )
import Prelude hiding (id)
import Servant ( throwError )
import Utils.Jwt (generateToken)
import Models.Jwt (TokenData(TokenData, email, id), Token)

type UserAPI =
    "register" :> ReqBody '[JSON] User :> Post '[JSON] Token
        :<|> "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] Token


userAPI :: Proxy UserAPI
userAPI = Proxy :: Proxy UserAPI


registerHandler :: ConnectionString -> User -> Handler Token
registerHandler connString user = do
  hashedPassword <- liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy (encodeUtf8 $ userPassword user)
  idres <- liftIO $ createUser connString user{userPassword = decodeLatin1 $ fromMaybe "" hashedPassword}
  generateToken TokenData {id = idres, email = userEmail user}



loginHandler :: ConnectionString -> LoginRequest -> Handler Token
loginHandler connString loginRequest = do
    let email = loginEmail loginRequest
        password = loginPassword loginRequest
    userEntity <- liftIO $ fetchUserByEmail connString email
    case userEntity of
      Just user -> do
        let hashedPassword = userPassword (entityVal user)
        if validatePassword (encodeUtf8   hashedPassword) (encodeUtf8 password) 
          then generateToken TokenData {id = fromSqlKey $ entityKey user, email = userEmail (entityVal user)}
          else throwError err401 { errBody = "Invalid Credentials" }
      Nothing -> throwError err401 { errBody = "Invalid Credentials" }

          
usersServer :: ConnectionString -> Server UserAPI
usersServer connString =
  registerHandler connString :<|> loginHandler connString 



