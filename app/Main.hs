{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Aeson
import Data.Text
import Data.Time (UTCTime (..), fromGregorian)
import GHC.Generics
import Network.Wai.Handler.Warp (Port, run)
import Servant
import Servant.API

type UserAPI = "users" :> Get '[JSON] [User]

data User = User
  { name :: String,
    age :: Int,
    email :: String,
    registration_date :: UTCTime
  }
  deriving (Eq, Show, Generic)

instance ToJSON User

users :: [User]
users =
  [ User "Isaac Newton" 372 "isaac@newton.co.uk" (UTCTime (fromGregorian 1683 3 1) 0),
    User "Albert Einstein" 136 "ae@mc2.org" (UTCTime (fromGregorian 1905 12 1) 0)
  ]

server :: Server UserAPI
server = pure users

userAPI :: Proxy UserAPI
userAPI = Proxy

app :: Application
app = serve userAPI server

port :: Port
port = 8081

main :: IO ()
main = do
  print $ "Starting server at " ++ show port
  run port app
