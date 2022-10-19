module Db
  ( CalendarDb (..),
    UserT (..),
    User,
    UserPassword,
    maximumUserPasswordLength,
    getUserId,
    initDb,
    getUserById,
    getUserByEmail,
    createUser,
  )
where

import Db.Connection
import Db.Scheme
import Db.Users
