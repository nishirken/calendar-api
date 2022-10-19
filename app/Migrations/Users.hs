{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Migrations.Users where

import Data.Password.Bcrypt (PasswordHash)
import Database.Beam
import Database.Beam.Backend
import Database.Beam.Migrate
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax
import Db

instance BeamMigrateSqlBackend be => HasDefaultSqlDataType be UserPassword where
  defaultSqlDataType _ _ _ = varCharType (Just maximumUserPasswordLength) Nothing

passwordHashDataType :: DataType Postgres UserPassword
passwordHashDataType = DataType (varCharType (Just maximumUserPasswordLength) Nothing)

createTableUsers =
  createTable "users" $
    User
      { _userId = field "id" serial,
        _userEmail = field "email" (varchar Nothing) notNull unique,
        _userPassword = field "password" passwordHashDataType notNull
      }
