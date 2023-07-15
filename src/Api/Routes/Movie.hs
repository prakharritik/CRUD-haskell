{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Api.Routes.Movie where

import Control.Monad.Trans.Except (throwE)
import Models.Movie
import Database.Persist.Postgresql (ConnectionString, toSqlKey)
import Database.Persist (entityVal)
import Data.Proxy
import Servant.API
import Servant.Server
import Data.Int (Int64)
import Control.Monad.IO.Class (liftIO)
import DB (createMovie, fetchMovie, updateMovie, deleteMovie, fetchAllMovies)


import Prelude hiding (id)

import Servant.Auth.Server

import Models.Jwt (TokenData, id)
import Servant (throwError)

type MovieAPI =
    "movies" :>  Capture "movieid" Int64 :> Get '[JSON] Movie
    :<|> "movies" :> Get '[JSON] [Movie]
    :<|> "movies" :> Auth '[JWT] TokenData :> ReqBody '[JSON] MovieRequest :> Post '[JSON] Int64
    :<|> "movies" :> Auth '[JWT] TokenData :> Capture "movieid" Int64 :> ReqBody '[JSON] MovieRequest :> Put '[JSON] SuccessResponse
    :<|> "movies" :> Auth '[JWT] TokenData :> Capture "movieid" Int64 :> Delete '[JSON] SuccessResponse

movieAPI :: Proxy MovieAPI
movieAPI = Proxy :: Proxy MovieAPI

-- fetch handler
fetchMovieHandler :: ConnectionString  -> Int64 -> Handler Movie
fetchMovieHandler connString movieId = do
  maybeMovie <- liftIO $ fetchMovie connString movieId
  case maybeMovie of
    Just movie -> return movie
    Nothing -> throwError err404 { errBody = "Could not find movie with that ID" }

-- fetch all movies
fetchAllMoviesHandler ::  ConnectionString -> Handler [Movie]
fetchAllMoviesHandler connString = do
  movies <- liftIO $ fetchAllMovies connString
  let moviesresult = map entityVal movies
  return moviesresult

-- create movie
createMovieHandler :: ConnectionString -> AuthResult TokenData -> MovieRequest -> Handler Int64
createMovieHandler connString (Authenticated userToken) movieReq = liftIO $ createMovie connString Movie{movieTitle = title movieReq, movieDescription = description movieReq, movieUserId =toSqlKey (id userToken)}
createMovieHandler _ _ _ = throwError err401 {errBody = "Invalid Token"}

-- update handler
updateMovieHandler :: ConnectionString -> AuthResult TokenData -> Int64 -> MovieRequest -> Handler SuccessResponse
updateMovieHandler connString (Authenticated userToken) movieid movieReq = do
  maybeExistingMovie <- liftIO $ fetchMovie connString movieid
  case maybeExistingMovie of
    Just _ -> do
        _ <- liftIO $ updateMovie connString movieid Movie{movieTitle = title movieReq, movieDescription = description movieReq, movieUserId =toSqlKey (id userToken)}
        return SuccessResponse { message = "Update successful!" }
    Nothing -> throwError err404 { errBody = "Could not find movie with that ID" }
updateMovieHandler _ _ _ _ = throwError err401 {errBody = "Invalid Token"}

-- delete handler
deleteMovieHandler :: ConnectionString -> AuthResult TokenData -> Int64 -> Handler SuccessResponse
deleteMovieHandler connString (Authenticated userToken) movieid = do
  maybeExistingMovie <- liftIO $ fetchMovie connString movieid
  case maybeExistingMovie of
    Just movie -> do
        if movieUserId movie == toSqlKey (id userToken) then do
          _ <- liftIO $ deleteMovie connString movieid
          return SuccessResponse { message = "Deleted successful!" }
        else throwError err403 {errBody = "Permission Denied"}
    Nothing -> throwError err404 { errBody = "Could not find movie with that ID" }
deleteMovieHandler _ _ _ = throwError err401 {errBody = "Invalid Token"}


moviesServer :: ConnectionString -> Server MovieAPI
moviesServer connString =
    fetchMovieHandler connString 
    :<|> fetchAllMoviesHandler connString
    :<|> createMovieHandler connString 
    :<|> updateMovieHandler connString 
    :<|> deleteMovieHandler connString 



