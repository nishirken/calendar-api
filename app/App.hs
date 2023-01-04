{-# LANGUAGE OverloadedStrings #-}

module App where

import Config (Config (..))
import Controllers (API, api, genAuthServerContext)
import qualified Data.ByteString.UTF8 as BS
import Database.Beam.Postgres (Connection (..))
import Network.HTTP.Types (methodDelete, methodGet, methodPost, methodPut)
import Network.Wai (Application)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors, simpleCors, simpleCorsResourcePolicy)
import Servant (Proxy (..), serveWithContext)

api' :: Proxy API
api' = Proxy

app :: Config -> Connection -> Application
app config connection =
  let corsMiddleware = cors $ \_ ->
        Just $
          simpleCorsResourcePolicy
            { corsMethods = [methodGet, methodPost, methodPut, methodDelete],
              corsOrigins = Just ([BS.fromString $ appOrigin config], True),
              corsRequestHeaders = ["Content-Type"]
            }
   in corsMiddleware $ serveWithContext api' (genAuthServerContext config) $ api config connection
