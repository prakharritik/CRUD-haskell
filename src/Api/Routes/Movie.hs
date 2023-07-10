{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

module Api.Routes.Movie where

import Models.User
import Control.Monad.Trans.Except (throwE)
import Models.Movie
import Database.Persist.Postgresql (ConnectionString)
import Database.Persist (entityVal)
import Data.Proxy
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Servant.API
import Servant.Server
import Data.Int (Int64)
import Control.Monad.IO.Class (liftIO)
import DB (createMovie, fetchMovie, updateMovie, deleteMovie)


type MovieAPI =
    "movies" :> Capture "movieid" Int64 :> Get '[JSON] Movie
    :<|> "movies" :> ReqBody '[JSON] Movie :> Post '[JSON] Int64
    :<|> "movies" :> Capture "movieid" Int64 :> ReqBody '[JSON] Movie :> Put '[JSON] SuccessResponse
    :<|> "movies" :> Capture "movieid" Int64 :> Delete '[JSON] SuccessResponse

movieAPI :: Proxy MovieAPI
movieAPI = Proxy :: Proxy MovieAPI

-- fetch handler
fetchMoviesHandler :: ConnectionString -> Int64 -> Handler Movie
fetchMoviesHandler connString movieId = do
  maybeMovie <- liftIO $ fetchMovie connString movieId
  case maybeMovie of
    Just movie -> return movie
    Nothing -> Handler (throwE $ err401 { errBody = "Could not find movie with that ID" })

createMovieHandler :: ConnectionString -> Movie -> Handler Int64
createMovieHandler connString movie = liftIO $ createMovie connString movie

-- update handler
updateMovieHandler :: ConnectionString -> Int64 -> Movie -> Handler SuccessResponse
updateMovieHandler connString movieid movie = do
  maybeExistingMovie <- liftIO $ fetchMovie connString movieid
  case maybeExistingMovie of
    Just _ -> do
        _ <- liftIO $ updateMovie connString movieid movie
        return SuccessResponse { message = "Update successful!" }
    Nothing -> return SuccessResponse { message = "Could not find movie with that ID" }

-- delete handler
deleteMovieHandler :: ConnectionString -> Int64 -> Handler SuccessResponse
deleteMovieHandler connString movieid = do
  maybeExistingMovie <- liftIO $ fetchMovie connString movieid
  case maybeExistingMovie of
    Just _ -> do
        _ <- liftIO $ deleteMovie connString movieid
        return SuccessResponse { message = "Deleted successful!" }
    Nothing -> return SuccessResponse { message = "Could not find movie with that ID" }

moviesServer :: ConnectionString -> Server MovieAPI
moviesServer connString =
    fetchMoviesHandler connString :<|>
  createMovieHandler connString :<|>
  updateMovieHandler connString :<|>
  deleteMovieHandler connString



