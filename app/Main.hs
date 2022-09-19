{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import GHC.Generics
import Network.Wai.Handler.Warp (Port, run)
import Servant
import Servant.API
import Config (config, Config (..))
import Database.PostgreSQL.Simple (Connection)
import Db.Connection (initDb)
import Controllers.User

type API = UserAPI

server :: Config -> Connection -> Server API
server config connection = getUser config connection

api :: Proxy API
api = Proxy

app :: Connection -> Application
app connection = serve api $ server config connection

main :: IO ()
main = do
  let port = appPort config
  print $ "Starting server at " ++ show port
  connection <- initDb config
  run (fromIntegral port) $ app connection
