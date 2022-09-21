{-# LANGUAGE OverloadedStrings #-}

module Controllers.Auth.Password (validate, isValid) where

import Data.Password.Validate
import Data.Password.Bcrypt (Password, mkPassword)
import Data.Text
import Control.Monad (when)
import Data.Either (isLeft)

policy :: PasswordPolicy
policy =
  PasswordPolicy
    { minimumLength = 5
    , maximumLength = 70
    , uppercaseChars = 1
    , lowercaseChars = 1
    , specialChars = 1
    , digitChars = 1
    , charSetPredicate = defaultCharSetPredicate
    }

validate :: Text -> ValidationResult
validate password = let validPolicy = validatePasswordPolicy policy in
  case validPolicy of
    Left errors -> error ("Invalid password policy " ++ show errors)
    Right validPolicy' -> validatePassword validPolicy' $ mkPassword password

isValid :: ValidationResult -> Bool
isValid ValidPassword = True
isValid _ = False

