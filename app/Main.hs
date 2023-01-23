module Main where

import App (api', app)
import Config (Config (..), getConfig)
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Controllers
import Data.Aeson (toJSON)
import Database.PostgreSQL.Simple (Connection)
import Db (connectDb)
import GHC.Generics
import Logger (logStdOut)
import qualified Network.Wai.Handler.Warp as Warp
import Servant
import Servant.API
import Settings (settings)

main :: IO ()
main = do
  config <- getConfig ".env"
  let port = appPort config
  connection <- connectDb config
  print $ "Starting server at " ++ show port
  Warp.runSettings (settings config) $ app config connection
