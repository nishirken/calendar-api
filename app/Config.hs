module Config where

import Data.Word (Word16)

data Config = Config
  { appPort :: Word16,
    dbPort :: Word16,
    dbHost :: String,
    dbUser :: String,
    dbName :: String
  }
  deriving (Eq, Show)

config :: Config
config =
  Config
    { appPort = 8081,
      dbPort = 5555,
      dbHost = "localhost",
      dbUser = "postgres",
      dbName = "calendar"
    }
