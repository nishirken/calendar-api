{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Controllers (API, api, genAuthServerContext) where

import Config (Config (Config))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Controllers.Auth.AuthCheck
import Controllers.Auth.Signin
import Controllers.Auth.Signup
import Controllers.User
import Database.Beam.Postgres (Connection)
import qualified Db
import Network.Wai (Request)
import Servant
import Servant.API
import Servant.Server.Experimental.Auth (AuthHandler)

type AuthAPI = "auth" :> (SignupAPI :<|> SigninAPI)

type PublicAPI = AuthAPI

type PrivateAPI = AuthProtect "cookie-auth" :> UserAPI

type API = PublicAPI :<|> PrivateAPI

genAuthServerContext :: Config -> Context (AuthHandler' ': '[])
genAuthServerContext config = authHandler config :. EmptyContext

api :: Config -> Connection -> Server API
api config connection = publicAPI :<|> privateAPI
  where
    publicAPI = signup config connection :<|> signin config connection
    privateAPI = getUser config connection
