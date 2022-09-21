{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Controllers.Auth.Signup where

import Config (Config)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Controllers.Auth.Password (isValid, validate)
import Controllers.User (UserResponse (..))
import Data.Aeson (FromJSON)
import qualified Data.ByteString.Lazy.UTF8 as BSLazy
import qualified Data.ByteString.UTF8 as BS
import Data.Either (isLeft)
import Data.Int (Int32)
import Data.Maybe (isJust)
import Data.String.Interpolate (i)
import Data.Text (Text, unpack)
import Database.Beam (SqlValable (val_))
import Database.Beam.Backend (SqlSerial (unSerial))
import Database.PostgreSQL.Simple (Connection)
import qualified Db
import GHC.Generics
import Servant
import Servant.API
import qualified Text.Email.Validate as EmailValidate

type SignupAPI = "signup" :> ReqBody '[JSON] SignupRequestBody :> Post '[JSON] UserResponse

data SignupRequestBody = SignupRequestBody
  { email :: Text,
    password :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON SignupRequestBody

-- 1) if email is not valid, throws 422
-- 2) if user with email already exists, throws 409
-- 3) if password is not valid, throws 422
-- 4) otherwise creates a new user
signup :: Config -> Connection -> SignupRequestBody -> Handler UserResponse
signup config connection SignupRequestBody {..} = do
  validEmail <-
    either
      (\reason -> throwError $ err422 {errBody = BSLazy.fromString [i|Email is not valid. #{reason}|]})
      (\email' -> pure email')
      $ EmailValidate.validate
      $ (BS.fromString . unpack) email

  user <- liftIO $ Db.getUserByEmail connection email
  when (isJust user) $ throwError $ err409 {errBody = BSLazy.fromString [i|User with email=#{email} already exists|]}

  let passwordValidationResult = validate password
  when (not $ isValid passwordValidationResult) $
    throwError $
      err422 {errBody = BSLazy.fromString [i|Password is not valid. #{passwordValidationResult}|]}
  newUser <- liftIO $ Db.createUser connection email password

  pure $ UserResponse ((unSerial . Db._userId) newUser) (Db._userEmail newUser)
