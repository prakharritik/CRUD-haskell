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

module Models.User where

import Data.Text (Text)
import Database.Persist.TH
import Data.Aeson
import Data.Aeson.Types (Parser, withObject, (.:), (.=))
import GHC.Generics (Generic)

share [mkPersist sqlSettings, mkMigrate "migrateUser"] [persistLowerCase|
  User sql=users
    name Text
    email Text
    password Text
    UniqueEmail email
    deriving Show Read
|]

instance ToJSON User where
  toJSON user = object 
    [ "name" .= userName user
    , "email" .= userEmail user
    , "password" .= userPassword user
    ]

instance FromJSON User where
  parseJSON = withObject "User" parseUser

parseUser :: Object -> Parser User
parseUser obj = do
  uName <- obj .: "name"
  uEmail <- obj .: "email"
  uPassword <- obj .: "password"
  return User
    { userName = uName
    , userEmail = uEmail
    , userPassword = uPassword
    }

data LoginRequest = LoginRequest
  { loginEmail :: Text
  , loginPassword :: Text
  } deriving (Generic)

instance FromJSON LoginRequest where
  parseJSON = withObject "LoginRequest" $ \v ->
    LoginRequest <$> v .: "email" <*> v .: "password"