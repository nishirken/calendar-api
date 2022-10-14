{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Config where

import Data.Word (Word16)
import Data.Aeson (FromJSON, fromJSON, decodeFileStrict')
import GHC.Generics (Generic)

data Config = Config
  { appPort :: Word16,
    dbPort :: Word16,
    dbHost :: String,
    dbUser :: String,
    dbName :: String,
    authKey :: String
  }
  deriving (Eq, Show, FromJSON, Generic)

getConfig :: IO Config
getConfig = do
  res <- (decodeFileStrict' "config.json" :: IO (Maybe Config))
  case res of
    (Just config) -> pure config
    Nothing -> error "Failed to decode config"
