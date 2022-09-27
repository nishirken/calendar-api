{-# LANGUAGE OverloadedStrings #-}

module Controllers.Auth.JWT (encodeJWT, verifyJWT, expirationDays) where

import Config (Config (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Lazy.UTF8 (ByteString)
import Data.Int (Int32)
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8)
import Data.Time (NominalDiffTime, nominalDay)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Generics (Generic)
import Web.JWT
import Prelude hiding (exp)

expirationDays :: NominalDiffTime
expirationDays = 30 * nominalDay

encodeJWT :: Text -> Int32 -> IO Text
encodeJWT secret userId = do
  nominalDiffTime <- getPOSIXTime
  let header =
        JOSEHeader
          { typ = Just "jwt",
            cty = Nothing,
            alg = Just $ HS256,
            kid = Nothing
          }
      claims =
        mempty
          { iss = (stringOrURI . pack) "calendar-api",
            aud = Left <$> (stringOrURI . pack) "calendar-web",
            sub = (stringOrURI . pack . show) userId,
            iat = numericDate nominalDiffTime,
            exp = numericDate $ nominalDiffTime + expirationDays
          }

  pure $ encodeSigned (encodeSigner secret) header claims

encodeSigner :: Text -> EncodeSigner
encodeSigner secret = hmacSecret secret

verifyJWT :: Text -> Text -> Maybe (JWT VerifiedJWT)
verifyJWT secret rawJWT = decodeAndVerifySignature signer rawJWT
  where
    signer = (toVerify . encodeSigner) secret
