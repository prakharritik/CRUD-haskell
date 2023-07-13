{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Models.Movie where

import Data.Text (Text)
import Database.Persist.TH
import Database.Persist.Sql (Key)
import Data.Aeson
import Data.Aeson.Types (Parser, withObject, (.:), (.=))
import GHC.Generics (Generic)

import Models.User ( UserId )

share [mkPersist sqlSettings, mkMigrate "migrateMovie"] [persistLowerCase|
Movie sql = movies
    title Text
    description Text
    userId (UserId) OnDeleteCascade OnUpdateCascade
    deriving Show Read
|]

-- Foreign user fk_user_movie OnDeleteCascade OnUpdateCascade userId references id

--  to json from json
instance ToJSON Movie where
  toJSON :: Movie -> Value
  toJSON movie = object 
    [ "title" .= movieTitle movie
    , "description" .= movieDescription movie
    , "userId" .= movieUserId movie
    ]


instance FromJSON Movie where
  parseJSON :: Value -> Parser Movie
  parseJSON = withObject "Movie" parseMovie

parseMovie :: Object -> Parser Movie
parseMovie obj = do
  mTitle <- obj .: "title"
  mDescription <- obj .: "description"
  mUserId <- obj .: "userId"
  return Movie
    { movieTitle = mTitle
    , movieDescription = mDescription
    , movieUserId = mUserId
    }

-- response type
data SuccessResponse = SuccessResponse
  {
    message :: String
  } deriving (Generic, Show)

instance ToJSON SuccessResponse
instance FromJSON SuccessResponse