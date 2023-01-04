{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module SigninSpec where

import App (app)
import Config (Config (..), getConfig, getEnvFileConfig)
import Controllers (API)
import Controllers.Auth.JWT (encodeJWT)
import Controllers.Auth.Signin (SigninAPI, SigninError (AuthCredsInvalid), SigninRequestBody (SigninRequestBody), SigninResponse (..), signin)
import Data.Aeson (Value, encode, toJSON)
import Data.ByteString (ByteString)
import Data.Maybe (isJust)
import Data.Sequence (fromList)
import Data.String.Interpolate (i)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Database.Beam.Postgres (Connection)
import qualified Db
import Debug.Trace (traceShow)
import Migrations.Migrate (migrateDb)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (Header, Status (..), http20, methodPost)
import Network.HTTP.Types.Header (hContentType, hSetCookie)
import qualified Network.Wai.Handler.Warp as Warp
import ResponseError (mkServerError)
import Servant (Proxy (..), getHeaders)
import Servant.Client (BaseUrl (..), ClientEnv (manager), ClientError (..), ResponseF (..), client, mkClientEnv, parseBaseUrl, runClientM)
import Servant.Server
import Settings (settings)
import Test.Hspec (Spec, context, describe, it, runIO)
import Test.Hspec.Wai (Body, MatchHeader (..), ResponseMatcher (..), liftIO, post, request, shouldRespondWith, with)
import Test.Hspec.Wai.JSON (json)
import Utils (cleanUp, withEnv)
import Web.Cookie (SetCookie (..), defaultSetCookie, parseCookies)

withApp :: Config -> Connection -> (Warp.Port -> IO ()) -> IO ()
withApp config connection =
  Warp.testWithApplicationSettings (settings config) (pure $ app config connection)

signinSpec :: Config -> Spec
signinSpec config = do
  connection <- runIO $ Db.connectDb config
  runIO $ migrateDb connection

  describe "/auth/signin" $ do
    with (pure $ app config connection) $ context "without cors" $ do
      let signinRequest email password =
            request
              methodPost
              "/auth/signin"
              [(hContentType, "application/json")]
              $ encode
              $ toJSON
              $ SigninRequestBody email password

      it "Throws 400 if user not exists" $ do
        let email = "d@mail.com"
            expected = [json|{code: "CREDENTIALS_INVALID"}|] {matchStatus = 400}
        signinRequest email "Q12345" `shouldRespondWith` expected

      it "Throws 400 if password not valid" $ do
        let email = "d12345@mail.com"
            password = "Q12345!@w"
            expected = [json|{code: "CREDENTIALS_INVALID"}|] {matchStatus = 400}
        liftIO $ Db.createUser connection email password
        signinRequest email "Q12345" `shouldRespondWith` expected

      it "Responds 200 if creds are valid" $ do
        let email = "d@mail.com"
            password = "Q12345!@w"
        liftIO $ Db.createUser connection email password
        signinRequest email password `shouldRespondWith` 200

-- TODO: mock jwt or currentTime somehow
--  it "Has jwt token in response" $ do
--    let
--      email = "dd@mail.com"
--      password = "Q12345!@w"
--      headerMatcher :: ByteString -> [Header] -> Body -> Maybe String
--      headerMatcher expectedJWT headers _ = do
--        cookie <- lookup hSetCookie headers
--        jwt <- lookup "jwt" $ parseCookies cookie
--        if jwt == expectedJWT then Nothing else Just [i|Failed to match expected jwt=#{expectedJWT}. |]
--      expected jwt = 200 { matchHeaders = [MatchHeader $ headerMatcher jwt] }
--    user <- liftIO $ Db.createUser connection email password
--    expectedJWT <- liftIO $ encodeJWT (pack $ authKey config) (Db.getUserId user)
--    signinRequest email password `shouldRespondWith` (expected $ encodeUtf8 expectedJWT)

-- around (withApp config connection) $ do
--  let
--    signinRequest = client (Proxy :: Proxy SigninAPI)
--    email = "dddd@mail.com"
--    password = "Q12345!@w"

--  baseUrl <- runIO $ parseBaseUrl "http://127.0.0.1/auth"
--  manager <- runIO $ newManager defaultManagerSettings
--  user <- runIO $ Db.createUser connection email password

--  context "Cors" $ do
--    it "Works with cors" $ \port -> do
--      let clientEnv = mkClientEnv manager baseUrl { baseUrlPort = port }
--      Right response <- runClientM (signinRequest $ SigninRequestBody email password) clientEnv
--      let
--        jwt :: Maybe ByteString
--        jwt = do
--          cookie <- lookup "Set-Cookie" $ getHeaders response
--          lookup "jwt" $ parseCookies cookie
--      expectedJWT <- liftIO $ encodeJWT (pack $ authKey config) (Db.getUserId user)
--      jwt `shouldBe` encodeUtf8 <$> Just expectedJWT
