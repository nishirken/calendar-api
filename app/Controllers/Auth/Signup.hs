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
import Data.Aeson (FromJSON, ToJSON (toJSON), encode, object, (.=))
import qualified Data.ByteString.Lazy.UTF8 as BSLazy
import qualified Data.ByteString.UTF8 as BS
import Data.Either (isLeft)
import Data.Int (Int32)
import Data.Maybe (isJust)
import Data.Password.Validate (InvalidReason, ValidationResult (..))
import Data.String.Interpolate (i)
import Data.Text (Text, unpack)
import Database.Beam (SqlValable (val_))
import Database.Beam.Backend (SqlSerial (unSerial))
import Database.PostgreSQL.Simple (Connection)
import qualified Db
import GHC.Generics
import ResponseError (ToServerError (..), mkServerError)
import Servant
import Servant.API
import qualified Text.Email.Validate as EmailValidate

type SignupAPI =
  "signup"
    :> ReqBody '[JSON] SignupRequestBody
    :> Post '[JSON] UserResponse

data SignupRequestBody = SignupRequestBody
  { email :: Text,
    password :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON SignupRequestBody

data SignupError
  = EmailInvalid String
  | UserExists
  | PasswordInvalid [InvalidReason]
  deriving (Eq, Show)

instance ToServerError SignupError where
  toErrorCode (EmailInvalid _) = "EMAIL_INVALID"
  toErrorCode UserExists = "USER_EXISTS"
  toErrorCode (PasswordInvalid _) = "PASSWORD_INVALID"

  toErrorBody (EmailInvalid reason) = Just $ object ["reason" .= reason]
  toErrorBody UserExists = Nothing
  toErrorBody (PasswordInvalid reason) = Just $ toJSON reason

  toBaseError (EmailInvalid _) = err422
  toBaseError UserExists = err409
  toBaseError (PasswordInvalid _) = err422

-- 1) if email is not valid, throws 422
-- 2) if user with email already exists, throws 409
-- 3) if password is not valid, throws 422
-- 4) otherwise creates a new user
signup :: Config -> Connection -> SignupRequestBody -> Handler UserResponse
signup config connection SignupRequestBody {..} = do
  validEmail <-
    either
      (\reason -> throwError $ mkServerError $ EmailInvalid reason)
      (\email' -> pure email')
      $ EmailValidate.validate
      $ (BS.fromString . unpack) email

  user <- liftIO $ Db.getUserByEmail connection email
  when (isJust user) $ throwError $ mkServerError UserExists

  let passwordValidationResult = validate password
  case passwordValidationResult of
    (InvalidPassword reasons) ->
      throwError $ mkServerError $ PasswordInvalid reasons
    ValidPassword -> do
      newUser <- liftIO $ Db.createUser connection email password
      pure $ UserResponse ((unSerial . Db._userId) newUser) (Db._userEmail newUser)
