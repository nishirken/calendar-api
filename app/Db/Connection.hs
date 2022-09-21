{-# LANGUAGE RecordWildCards #-}

module Db.Connection where

import Config
import Database.Beam.Postgres

initDb :: Config -> IO Connection
initDb Config {..} = connect $ ConnectInfo dbHost dbPort dbUser "" dbName
