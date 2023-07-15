{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils.Jwt where

import Prelude hiding (id)
import Data.Text (pack)
import Servant
    ( err401, err500, throwError, Handler, ServerError(errBody) )
import Servant.Auth.Server
    ( fromSecret,
      defaultJWTSettings,
      makeJWT,
      AuthResult(Authenticated) )
import Control.Monad.Cont (MonadIO(liftIO))
import qualified Data.ByteString.Lazy as BSL
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Models.Jwt (TokenData, Token (Token, jwtToken))


-- Handler for the API endpoint
getUser :: AuthResult TokenData -> Handler TokenData
getUser authResult = case authResult of
  Authenticated user -> return  user
  _               -> throwError err401



generateToken :: TokenData -> Handler Token
generateToken user = do
  let jwtSettings = defaultJWTSettings (fromSecret "g8Z00Hbp7ZP4XQ_TiYDpRHgKHhcp5AZS-CWLMEknBxTuuy4bTJLuzZGPvk1bWWuZ-plRaK6_NxtBwHjk5e1dHmFM6zZKYAOAU5ZPHr_pI-E8YkY7iIDtaHOmtPy1pJwDsXjpPeGftpd")
  eToken <- liftIO $ makeJWT user jwtSettings Nothing
  case eToken of
    Left err    -> throwError err500 { errBody = BSL.fromStrict (encodeUtf8 $ pack (show err)) }
    Right token -> return $ Token { jwtToken = decodeUtf8 (BSL.toStrict token) }
