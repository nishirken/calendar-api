{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Config (Config (..), config)
import Control.Exception (SomeException (SomeException), catches, try)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Controllers
import Data.ByteString.Lazy.UTF8 (fromString)
import Database.PostgreSQL.Simple (Connection)
import Db.Connection (initDb)
import GHC.Generics
import Network.HTTP.Types.Status (status500)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Servant
import Servant.API

api' :: Proxy API
api' = Proxy

app :: Connection -> Wai.Application
app connection = serveWithContext api' (genAuthServerContext config) $ api config connection

main :: IO ()
main = do
  let port = appPort config
  print $ "Starting server at " ++ show port
  connection <- initDb config
  let response500 :: SomeException -> Wai.Response
      response500 err = Wai.responseLBS status500 [] (fromString $ "Something went wrong. " <> show err)
      settings =
        Warp.setPort (fromIntegral port) $
          Warp.setOnExceptionResponse response500 $
            Warp.defaultSettings
  Warp.runSettings settings $ app connection
