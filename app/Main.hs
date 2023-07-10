{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Data.Proxy
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Servant.API
import Servant.Server

import Api.Routes.User
import Api.Routes.Movie
import DB (parseConnectionString, runMigrations)
-- import DB (connectToDB, runMigrations)

type HelloAPI = "hello" :> Get '[PlainText] String
type API = HelloAPI :<|> UserAPI :<|> MovieAPI 

server :: Server HelloAPI
server = return "Hello, World!"

server1 :: Server API
server1 = server :<|> (usersServer parseConnectionString) :<|> (moviesServer parseConnectionString)

api :: Proxy API
api = Proxy

app :: Application
app = serve api (server1)

main :: IO ()
main = do
    -- pool <- connectToDB (dbConnectionString config)
    runMigrations parseConnectionString
    run 8080 app
    