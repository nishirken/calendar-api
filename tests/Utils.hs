{-# LANGUAGE QuasiQuotes #-}

module Utils (withEnv, cleanUp) where

import Config (Config (..), getEnvFileConfig)
import Data.String.Interpolate (i)
import Data.Text (pack, unpack)
import Database.Beam.Postgres (Connection)
import Db.Connection (connectDb)
import Migrations.Migrate (migrateDb)
import qualified Shelly as Sh
import System.FilePath ((</>))

pgDir :: FilePath
pgDir = ".tmp/test_db"

envFile :: FilePath
envFile = ".env"

withEnv :: IO (Config, Connection)
withEnv = do
  config <- getEnvFileConfig envFile
  connection <- connectDb config
  migrateDb connection
  pure (config, connection)

cleanUp :: Config -> IO ()
cleanUp config = Sh.shelly $ do
  Sh.bash_
    ""
    [ [i|psql -U #{dbUser config} -p #{dbPort config} -h #{dbHost config} -d #{dbName config} -c "DROP OWNED BY #{dbUser config} CASCADE;"|]
    ]
