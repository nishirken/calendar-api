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

module Db.Users where

import Data.Functor.Identity (Identity (Identity))
import Data.Int (Int32)
import Data.Password.Bcrypt (Bcrypt, PasswordHash (..))
import Data.Text (Text, unpack)
import Database.Beam
import Database.Beam.Backend (HasSqlValueSyntax (sqlValueSyntax), autoSqlValueSyntax)
import Database.Beam.Backend.SQL.AST (Value (Value))
import Database.Beam.Backend.SQL.Types (SqlSerial)
import Database.Beam.Postgres

data UserT f = User
  { _userId :: Columnar f (SqlSerial Int32),
    _userEmail :: Columnar f Text,
    _userPassword :: Columnar f UserPassword
  }
  deriving (Generic)

type User = UserT Identity

type UserId = PrimaryKey UserT Identity

type UserPassword = PasswordHash Bcrypt

deriving instance Show User

deriving instance Eq User

instance Beamable UserT

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be UserPassword where
  sqlValueSyntax password = sqlValueSyntax $ unPasswordHash password

instance FromBackendRow Postgres UserPassword where
  fromBackendRow = PasswordHash <$> fromBackendRow

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f (SqlSerial Int32)) deriving (Generic, Beamable)
  primaryKey = UserId . _userId
