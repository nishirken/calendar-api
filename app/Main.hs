{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import GHC.Generics
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as Wai
import Servant
import Servant.API
import Config (config, Config (..))
import Database.PostgreSQL.Simple (Connection)
import Db.Connection (initDb)
import Controllers.User (UserAPI, getUser)
import Controllers.Auth (AuthAPI, auth)
import Control.Exception (try, catches, SomeException (SomeException))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.ByteString.Lazy.UTF8 (fromString)
import Network.HTTP.Types.Status (status500)

type API = UserAPI :<|> AuthAPI

server :: Config -> Connection -> Server API
server config connection =
  getUser config connection
  :<|> auth config connection

api :: Proxy API
api = Proxy

app :: Connection -> Wai.Application
app connection = serve api $ server config connection

main :: IO ()
main = do
  let port = appPort config
  print $ "Starting server at " ++ show port
  connection <- initDb config
  let
    response500 :: SomeException -> Wai.Response
    response500 err = Wai.responseLBS status500 [] (fromString $ "Something went wrong. " <> show err)
    settings =
      Warp.setPort (fromIntegral port) $
      Warp.setOnExceptionResponse response500 $
      Warp.defaultSettings
  Warp.runSettings settings $ app connection
