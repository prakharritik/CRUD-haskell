{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

module Api.Routes.User where

import Models.User
import Database.Persist.Postgresql (ConnectionString, Entity (entityKey), fromSqlKey)
import Database.Persist (entityVal, entityVal)
import Control.Monad.Logger (runStdoutLoggingT, LoggingT, logInfoN)

import Data.Proxy
import Data.Maybe (fromMaybe)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Servant.API
import Servant.Server
import Data.Int (Int64)
import Data.Text.Encoding
import Web.JWT
import Control.Monad.IO.Class (liftIO)
import DB (createUser, fetchUserByEmail)
import Crypto.BCrypt
-- import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text.Encoding as TE
import System.Random
import Control.Monad.Logger (runStdoutLoggingT, LoggingT, logInfoN)

import Data.Text (Text, unpack)
import Data.Aeson( Value( Number ), Value(String), FromJSON )
import Data.Aeson
import GHC.Generics (Generic)
import Control.Monad.Trans.Except (throwE)

-- import Servant.Auth.Server

import Data.Map (Map)

import qualified Data.Map as Map


import Data.ByteString.Char8


type UserAPI =
    "users" :> ReqBody '[JSON] User :> Post '[JSON] Text
        :<|> "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] Text

-- type ProtectedAPI = "protected" :> Get '[JSON] String

-- type AApi = (ProtectedAPI :<|> UserAPI)



-- protectedEndpoint :: JWTClaimsSet -> Handler String
-- protectedEndpoint claims = return $ "Protected endpoint: User ID - " ++ show (23)

-- authMiddleware :: Middleware
-- authMiddleware app req respond = do
--   let maybeToken = extractBearerToken req
--   case maybeToken of
--     Just token -> do
--       let maybeClaims = verifyAndDecodeToken token
--       case maybeClaims of
--         Just claims -> app (addClaimsToRequest claims req) respond
--         Nothing -> "unauthorized respond"
--     Nothing -> "unauthorized respond"
--   where
--     extractBearerToken req = lookup "Authorization" (requestHeaders req) >>= extractToken
--     extractToken = decodeLatin1 .stripPrefix "Bearer " .  encodeUtf8
--     verifyAndDecodeToken token = decodeAndVerifySignature (secret "secret-key") token
--     addClaimsToRequest claims req = req { vault = insert "jwtClaims" claims (vault req) }
    -- unauthorized respond = respond $ responseLBS unauthorized401 [] "Unauthorized"

-- getClaim :: String -> JWTClaimsSet -> Maybe Claim
-- getClaim claimName claimsSet = lookup (fromString claimName) (unClaimsMap $ jwtClaimsSetClaims claimsSet)

-- userIdFromClaims :: JWTClaimsSet -> Int
-- userIdFromClaims claims =
--   case getClaim "ID" claims of
--     Just (Number userId) -> floor userId
--     _ -> error "Invalid or missing user ID claim"

userAPI :: Proxy UserAPI
userAPI = Proxy :: Proxy UserAPI

createUserHandler :: ConnectionString -> User -> Handler Text
createUserHandler connString user = do
  hashedPassword <- liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy (encodeUtf8 $ userPassword user)
  idres <- liftIO $ createUser connString user{userPassword = decodeLatin1 $ fromMaybe "" hashedPassword}
  let cs = mempty { -- mempty returns a default JWTClaimsSet
unregisteredClaims = ClaimsMap $ Map.fromList
  [ (decodeLatin1 "email", String $ userEmail user)
  , (decodeLatin1 "ID", Number $ fromIntegral idres)
  ]
      }
      key = hmacSecret $ decodeUtf8 "secret-key"
  return $  encodeSigned key mempty cs


loginHandler :: ConnectionString -> LoginRequest -> Handler Text
loginHandler connString loginRequest = do
   runStdoutLoggingT $ do
    logInfoN "Starting application..."
    let email = loginEmail loginRequest
        password = loginPassword loginRequest
    userEntity <- liftIO $ fetchUserByEmail connString email
    case userEntity of
      Just user -> do
        let u = entityVal user
        let hashedPassword = userPassword u
        if validatePassword (encodeUtf8   hashedPassword) (encodeUtf8 password) 
          then do
            let cs = mempty { -- mempty returns a default JWTClaimsSet
                  unregisteredClaims = ClaimsMap $ Map.fromList
                    [ (decodeLatin1 "email", String $ userEmail u)
                    , (decodeLatin1 "ID", Number $ fromIntegral (fromSqlKey $ entityKey user))
                    ]
                }
                key = hmacSecret $ decodeUtf8 "secret-key"
                token = encodeSigned key mempty cs
            return token
        else return "Invalid Credentials"
      Nothing -> return "Invalid Credentials"
          
usersServer :: ConnectionString -> Server UserAPI
usersServer connString =
  createUserHandler connString :<|> loginHandler connString



