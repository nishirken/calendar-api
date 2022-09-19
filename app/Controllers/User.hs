{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Controllers.User where

import qualified Db
import GHC.Generics
import Servant
import Servant.API
import Data.Int (Int32)
import Data.Text (Text)
import Data.Aeson (ToJSON)
import Config (Config)
import Database.PostgreSQL.Simple (Connection)
import Control.Monad.IO.Class (MonadIO(liftIO))

type UserAPI = "user" :> Capture "id" Int32 :> Get '[JSON] UserResponse

data UserResponse = UserResponse
  { id :: Int32
  , email :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON UserResponse

getUser :: Config -> Connection -> Int32 -> Handler UserResponse
getUser config connection userId = do
  user <- liftIO $ Db.getUserById connection userId
  case user of
    Just user' -> pure $ UserResponse (Db._userId user') (Db._userEmail user')
    Nothing -> throwError $ err404

