{-# LANGUAGE TemplateHaskell #-}

module DB where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStdoutLoggingT, LoggingT)
import Database.Persist
import Database.Persist.Postgresql
import Data.ByteString.Char8
import Control.Monad.Reader (runReaderT)
import Data.Int (Int64)
import Data.Text (Text)


import Models.User
import Models.Movie

type Database = SqlPersistT IO

parseConnectionString :: ConnectionString
parseConnectionString = pack "host=127.0.0.1 port=5432 user=postgres dbname=CRUD password=root"

runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a -> IO a
runAction connectionString action = 
  runStdoutLoggingT $ withPostgresqlConn connectionString $ \backend ->
    runReaderT action backend

-- migrate function to generate users table and migrate the DB 
runMigrations :: ConnectionString -> IO ()
runMigrations connString = do
  runAction connString $ do
    runMigration migrateUser
    runMigration migrateMovie

-- -- User-related database functions

createUser :: ConnectionString -> User -> IO Int64
createUser connString user = fromSqlKey <$> runAction connString (insert user)

-- Fetching a user by email
fetchUserByEmail :: ConnectionString -> Text -> IO (Maybe (Entity User))
fetchUserByEmail connString email = runStdoutLoggingT $ do
  -- Wrap the database operation in SqlPersistT and LoggingT
  liftIO $ runAction connString $ do
    user <- getBy (UniqueEmail email)
    return user

-- fetchUserById :: ConnectionString -> Int64 -> IO (Maybe  AuthenticatedUser)
-- fetchUserById connString id = do 
--   u <- runAction connString (get (toSqlKey id))
--   return $ case u of
--     Just u -> Just (AUser (   fromIntegral (fromSqlKey $ entityKey u)) (unpack $ encodeUtf8 $ userEmail (entityVal u)))
--     Nothing -> Nothing
-- -- Movie-related database functions

-- fetching a movie from DB
fetchMovie :: ConnectionString -> Int64 -> IO (Maybe Movie)
fetchMovie connString uid = runAction connString (get (toSqlKey uid))

-- Creating a movie
createMovie :: ConnectionString -> Movie -> IO Int64
createMovie connString movie = fromSqlKey <$> runAction connString (insert movie)
    
updateMovie :: ConnectionString -> Int64 -> Movie -> IO SuccessResponse
updateMovie connString uid movie = do
    let movieKey = toSqlKey uid
    _ <- runAction connString (replace movieKey movie)
    return SuccessResponse { message = "Update successful!" }

-- Deleting a movie
deleteMovie :: ConnectionString -> Int64 -> IO ()
deleteMovie connString uid = runAction connString (delete movieKey)
  where
    movieKey :: Key Movie
    movieKey = toSqlKey uid

-- Fetching all movies
fetchAllMovies :: ConnectionString -> IO [Entity Movie]
fetchAllMovies connString = runAction connString $ selectList [] []