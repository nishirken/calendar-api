{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

module Config where

import Configuration.Dotenv (parseFile)
import Control.Monad ((<=<))
import Data.Either (fromLeft, fromRight)
import Data.String.Interpolate (i)
import Data.Word (Word16)
import qualified Env
import GHC.Generics (Generic)
import System.Directory (getCurrentDirectory)
import System.Environment (getEnvironment, getExecutablePath)
import System.FilePath ((</>))
import Text.Read (readEither)

data Config = Config
  { appPort :: Word16,
    appOrigin :: String,
    dbPort :: Word16,
    dbHost :: String,
    dbUser :: String,
    dbName :: String,
    dbPassword :: String,
    authKey :: String
  }
  deriving (Eq, Show)

class Read a => Parse' a where
  parse' :: String -> Either String a

instance Parse' Word16 where
  parse' = readEither

instance Parse' String where
  parse' val = readEither $ "\"" <> val <> "\""

instance Parse' Bool where
  parse' "true" = Right True
  parse' "false" = Right False
  parse' val = Left [i|Failed to parse bool for value: #{val}|]

parseVar :: Parse' a => [(String, String)] -> String -> Either String a
parseVar vars varName =
  case lookup varName vars of
    (Just value) -> case parse' value of
      (Right val) -> Right val
      (Left err) -> Left $ [i|Variable #{varName}=#{value} parse error. |] <> err
    Nothing -> Left [i|Variable #{varName} has not been set|]

readEither' rawVal = case readEither rawVal of
  (Right val) -> Right val
  (Left err) -> Left $ Env.UnreadError err

getEnvVarsConfig :: IO Config
getEnvVarsConfig = do
  Env.parse (Env.header "env config") $
    Config
      <$> Env.var (readEither' <=< Env.nonempty) "APP_PORT" mempty
      <*> Env.var (Env.str <=< Env.nonempty) "APP_ORIGIN" mempty
      <*> Env.var (readEither' <=< Env.nonempty) "DB_PORT" mempty
      <*> Env.var (Env.str <=< Env.nonempty) "DB_HOST" mempty
      <*> Env.var (Env.str <=< Env.nonempty) "POSTGRES_USER" mempty
      <*> Env.var (Env.str <=< Env.nonempty) "POSTGRES_DB" mempty
      <*> Env.var (Env.str <=< Env.nonempty) "POSTGRES_PASSWORD" mempty
      <*> Env.var (Env.str <=< Env.nonempty) "AUTH_KEY" mempty

getEnvFileConfig :: FilePath -> IO Config
getEnvFileConfig envFileName = do
  rawVars <- parseFile envFileName

  let parseVar' :: Parse' a => String -> Either String a
      parseVar' = parseVar rawVars
      res :: Either String Config
      res =
        Config
          <$> (parseVar' "APP_PORT" :: Either String Word16)
          <*> (parseVar' "APP_ORIGIN" :: Either String String)
          <*> (parseVar' "DB_PORT" :: Either String Word16)
          <*> (parseVar' "DB_HOST" :: Either String String)
          <*> (parseVar' "POSTGRES_USER" :: Either String String)
          <*> (parseVar' "POSTGRES_DB" :: Either String String)
          <*> (parseVar' "POSTGRES_PASSWORD" :: Either String String)
          <*> (parseVar' "AUTH_KEY" :: Either String String)

  case res of
    (Right config) -> pure config
    (Left err) -> error ("Failed to parse config. " <> err)

getConfig :: FilePath -> IO Config
getConfig envFileName = do
  envVars <- getEnvironment
  let isProd = fromRight False (parseVar envVars "PROD" :: Either String Bool)

  if isProd then getEnvVarsConfig else getEnvFileConfig envFileName
