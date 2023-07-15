{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.Jwt where

import Data.Text (Text)
import Data.Aeson ( FromJSON, ToJSON )
import Data.Int (Int64)
import GHC.Generics ( Generic )
import Servant.Auth.Server ( FromJWT, ToJWT )

-- Token data type
newtype Token = Token
  { jwtToken :: Text
  } deriving (Generic)

-- Instances for token data type
instance ToJSON Token
instance FromJSON Token


-- User data type
data TokenData = TokenData
  { id :: Int64
  , email :: Text
  } deriving (Generic)

-- Instances for user data type
instance ToJSON TokenData
instance FromJSON TokenData
instance ToJWT TokenData
instance FromJWT TokenData