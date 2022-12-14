{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Migrations.Migrate (migrateDb, verifyDbSchema) where

import Control.Arrow ((>>>))
import Database.Beam
import Database.Beam.Backend
import Database.Beam.Migrate
import Database.Beam.Migrate.Simple
import Database.Beam.Postgres
import Database.Beam.Postgres.Migrate
import Db (CalendarDb (..))
import Migrations.Users

calendarDbMigrationSettings :: CheckedDatabaseSettings Postgres CalendarDb
calendarDbMigrationSettings = defaultMigratableDbSettings

initialSetup :: Migration Postgres (CheckedDatabaseSettings Postgres CalendarDb)
initialSetup = do
  usersTable <- createTableUsers
  pure $
    CalendarDb
      { _calendarUsers = usersTable
      }

allowDestructive :: (Monad m, MonadFail m) => BringUpToDateHooks m
allowDestructive =
  defaultUpToDateHooks
    { runIrreversibleHook = pure True
    }

migrateDb ::
  Connection ->
  IO (Maybe (CheckedDatabaseSettings Postgres CalendarDb))
migrateDb conn =
  runBeamPostgresDebug putStrLn conn
    $ bringUpToDateWithHooks
      allowDestructive
      migrationBackend
    $ migrationStep "initial_setup" (const initialSetup)

verifyDbSchema :: Connection -> IO ()
verifyDbSchema conn = do
  res <- runBeamPostgres conn $ verifySchema migrationBackend calendarDbMigrationSettings
  case res of
    VerificationSucceeded -> print "The database has been successfully verified"
    VerificationFailed predicates -> error $ "The database schema has not been verified. " <> show predicates
