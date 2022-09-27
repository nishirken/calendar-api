{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Controllers.Auth.AuthCheck where

import Config (Config (..))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Controllers.Auth.JWT (verifyJWT)
import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import qualified Db
import Network.Wai (Request (requestHeaders))
import Servant
import Servant.API
import Servant.Server.Experimental.Auth
import Text.Read (readMaybe)
import Web.Cookie (parseCookies)
import Web.JWT

type instance AuthServerData (AuthProtect "cookie-auth") = Int32

type AuthHandler' = AuthHandler Request Int32

authHandler :: Config -> AuthHandler'
authHandler config = mkAuthHandler handler
  where
    maybeToEither e = maybe (Left e) Right
    throw401 msg = throwError $ err401 {errBody = msg}
    handler req = either throw401 lookupUserId $ do
      cookie <- maybeToEither "Missing cookie header" $ lookup "cookie" $ requestHeaders req
      maybeToEither "Missing jwt token in cookie" $ lookup "jwt" $ parseCookies cookie
    lookupUserId :: ByteString -> Handler Int32
    lookupUserId jwtCookie = either throw401 pure $ do
      verifiedJWT <- maybeToEither "Token unverified" $ verifyJWT (pack . authKey $ config) $ decodeUtf8 jwtCookie
      subject' <- maybeToEither "Token invalid, no subject in claims" $ (sub . claims) verifiedJWT
      maybeToEither "Token invalid, failed to parse subject in claims" $
        ((readMaybe :: String -> Maybe Int32) . unpack . stringOrURIToText) subject'
