{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Controllers.Auth.Signin where

import Config (Config (authKey))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Controllers.Auth.JWT (encodeJWT, expirationDays)
import Data.Aeson (FromJSON)
import Data.Maybe (isJust, isNothing)
import qualified Data.Password.Bcrypt as Password
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Database.Beam.Postgres (Connection)
import qualified Db
import GHC.Generics (Generic)
import Servant
import Servant.API
import Web.Cookie (SetCookie (..), defaultSetCookie, sameSiteStrict)

type SigninHeaders = Headers '[Header "Set-Cookie" SetCookie]

type SigninResponse = SigninHeaders ()

type SigninAPI = "signin" :> ReqBody '[JSON] SigninRequestBody :> Post '[JSON] SigninResponse

data SigninRequestBody = SigninRequestBody
  { email :: !Text,
    password :: !Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON SigninRequestBody

-- 1) Checks if email exists. If not, throws 400
-- 2) Checks if password valid. If not, throws 400
signin :: Config -> Connection -> SigninRequestBody -> Handler SigninResponse
signin config conn SigninRequestBody {..} = do
  let err = throwError $ err400 {errBody = "Email or password are invalid"}
      check Password.PasswordCheckSuccess = True
      check _ = False
  user <- liftIO $ Db.getUserByEmail conn email
  case user of
    (Just user') -> do
      let isPasswordValid = check $ Password.checkPassword (Password.mkPassword password) (Db._userPassword user')
      when (not isPasswordValid) err
      jwt <- liftIO $ encodeJWT (pack $ authKey config) (Db.getUserId user')
      let jwtCookie =
            defaultSetCookie
              { setCookieName = "jwt",
                setCookieValue = (encodeUtf8 jwt),
                setCookieMaxAge = Just $ realToFrac expirationDays,
                setCookieSecure = True,
                setCookieHttpOnly = True,
                setCookieSameSite = Just sameSiteStrict
              }
      pure $ addHeader jwtCookie ()
    Nothing -> err
