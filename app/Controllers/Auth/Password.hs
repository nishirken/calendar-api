{-# LANGUAGE OverloadedStrings #-}

module Controllers.Auth.Password (validate, isValid) where

import Control.Monad (when)
import Data.Aeson (ToJSON, object, toJSON, (.=))
import Data.Either (isLeft)
import Data.Password.Bcrypt (Password, mkPassword)
import Data.Password.Validate
import Data.Text
import Db (maximumUserPasswordLength)

policy :: PasswordPolicy
policy =
  PasswordPolicy
    { minimumLength = 5,
      maximumLength = fromIntegral maximumUserPasswordLength,
      uppercaseChars = 1,
      lowercaseChars = 1,
      specialChars = 1,
      digitChars = 1,
      charSetPredicate = defaultCharSetPredicate
    }

validate :: Text -> ValidationResult
validate password =
  let validPolicy = validatePasswordPolicy policy
   in case validPolicy of
        Left errors -> error ("Invalid password policy " ++ show errors)
        Right validPolicy' -> validatePassword validPolicy' $ mkPassword password

isValid :: ValidationResult -> Bool
isValid ValidPassword = True
isValid _ = False

instance ToJSON InvalidReason where
  toJSON (PasswordTooShort minLen providedLen) =
    object ["code" .= ("PASSWORD_TOO_SHORT" :: String), "minLength" .= minLen, "providedLength" .= providedLen]
  toJSON (PasswordTooLong maxLen providedLen) =
    object ["code" .= ("PASSWORD_TOO_LONG" :: String), "maxLength" .= maxLen, "providedLength" .= providedLen]
  toJSON (NotEnoughReqChars category minAmount providedAmount) =
    object
      [ "characterCategory" .= show category,
        "minimumAmount" .= minAmount,
        "providedAmount" .= providedAmount,
        "code" .= ("NOT_ENOUGH_REQ_CHARS" :: String)
      ]
  toJSON (InvalidCharacters text) = object ["code" .= ("INVALID_CHARS" :: String), "invalidCharacters" .= text]
