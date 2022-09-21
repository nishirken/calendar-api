{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Controllers.Auth.Signup where

import qualified Db
import GHC.Generics
import Servant
import Servant.API
import Data.Int (Int32)
import Data.Text (Text, unpack)
import Config (Config)
import Database.PostgreSQL.Simple (Connection)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Controllers.User (UserResponse(..))
import Data.String.Interpolate (i)
import Data.Maybe (isJust)
import Data.Either (isLeft)
import Controllers.Auth.Password (validate, isValid)
import Control.Monad (when) 
import Data.Aeson (FromJSON)
import Database.Beam (SqlValable(val_))
import qualified Data.ByteString.Lazy.UTF8 as BSLazy
import qualified Data.ByteString.UTF8 as BS
import qualified Text.Email.Validate as EmailValidate
import Database.Beam.Backend (SqlSerial(unSerial))

type SignupAPI = "signup" :> ReqBody '[JSON] SignupRequestBody :> Post '[JSON] UserResponse

data SignupRequestBody = SignupRequestBody
  { email :: Text
  , password :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON SignupRequestBody

-- 1) if email is not valid, throws 422
-- 2) if user with email already exists, throws 409
-- 3) if password is not valid, throws 422
-- 4) otherwise creates a new user
signup :: Config -> Connection -> SignupRequestBody -> Handler UserResponse
signup config connection SignupRequestBody{..} = do
  validEmail <- either
    (\reason -> throwError $ err422 { errBody = BSLazy.fromString [i|Email is not valid. #{reason}|] })
    (\email' -> pure email') $
    EmailValidate.validate $ (BS.fromString . unpack) email

  user <- liftIO $ Db.getUserByEmail connection email
  when (isJust user) $ throwError $ err409 { errBody = BSLazy.fromString [i|User with email=#{email} already exists|] }
  
  let passwordValidationResult = validate password
  when (not $ isValid passwordValidationResult) $ throwError $
    err422 { errBody = BSLazy.fromString [i|Password is not valid. #{passwordValidationResult}|] }
  newUser <- liftIO $ Db.createUser connection email password

  pure $ UserResponse ((unSerial . Db._userId) newUser) (Db._userEmail newUser)

