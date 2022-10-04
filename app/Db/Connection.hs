{-# LANGUAGE RecordWildCards #-}

module Db.Connection where

import Config
import Database.Beam.Postgres

initDb :: Config -> IO Connection
initDb Config {..} =
  connect $
    ConnectInfo
      { connectHost = dbHost,
        connectPort = dbPort,
        connectUser = dbUser,
        connectPassword = "12345",
        connectDatabase = dbName
      }
