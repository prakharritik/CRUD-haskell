{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (id)
import Data.Text (Text)
import GHC.Generics ( Generic )
import Network.Wai.Handler.Warp (run)
import Servant
import Data.Aeson ( FromJSON, ToJSON )
import Servant.Auth.Server
import Api.Routes.User
import qualified Data.ByteString.Char8 as BS
import Api.Routes.Movie
import DB (parseConnectionString)
import System.Environment
import Data.Maybe (fromMaybe)

-- Token data type
newtype Token = Token
  { jwtToken :: Text
  } deriving (Generic)

-- Instances for token data type
instance ToJSON Token
instance FromJSON Token

type WelcomeApi = Get '[PlainText] String
type API = WelcomeApi :<|> UserAPI :<|> MovieAPI 

welcomeServer :: Server WelcomeApi
welcomeServer = return "Welcome to Rotten Tomatoes XD!!"

server :: Server API
server = welcomeServer :<|> usersServer parseConnectionString :<|> moviesServer parseConnectionString


-- Server and JWT settings
app :: JWTSettings -> Application
app jwtCfg = serveWithContext api ctx server
  where
    api :: Proxy API
    api = Proxy

    ctx :: Context '[JWTSettings, CookieSettings]
    ctx = jwtCfg :. defaultCookieSettings :. EmptyContext

-- Main function to start the server
main :: IO ()
main = do
  jwtSecret <- lookupEnv "jwtSecret"

  case jwtSecret of
    Just secret -> do
      let jwtCfg = defaultJWTSettings (fromSecret $ BS.pack secret)
      run 8080 $ app jwtCfg

    Nothing ->
      putStrLn "jwtSecret is not set"
  
