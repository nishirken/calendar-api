{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Config where

import Data.Word (Word16)
import GHC.Generics (Generic)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import Text.Read (readEither)
import Control.Monad ((<=<))
import Configuration.Dotenv (parseFile)
import Data.String.Interpolate (i)
import System.Environment (getExecutablePath, getEnvironment)
import qualified Env
import Data.Either (fromRight, fromLeft)

data Config = Config
  { appPort :: Word16,
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
  Env.parse (Env.header "env config") $ Config
    <$> Env.var (readEither' <=< Env.nonempty) "APP_PORT" mempty
    <*> Env.var (readEither' <=< Env.nonempty) "DB_PORT" mempty
    <*> Env.var (Env.str <=< Env.nonempty) "DB_HOST" mempty
    <*> Env.var (Env.str <=< Env.nonempty) "POSTGRES_USER" mempty
    <*> Env.var (Env.str <=< Env.nonempty) "POSTGRES_DB" mempty
    <*> Env.var (Env.str <=< Env.nonempty) "POSTGRES_PASSWORD" mempty
    <*> Env.var (Env.str <=< Env.nonempty) "AUTH_KEY" mempty


getEnvFileConfig :: IO Config
getEnvFileConfig = do
  rawVars <- parseFile ".env"

  let
    parseVar' :: Parse' a => String -> Either String a
    parseVar' = parseVar rawVars
    res :: Either String Config
    res = Config
      <$> (parseVar' "APP_PORT" :: Either String Word16)
      <*> (parseVar' "DB_PORT" :: Either String Word16)
      <*> (parseVar' "DB_HOST" :: Either String String)
      <*> (parseVar' "POSTGRES_USER" :: Either String String)
      <*> (parseVar' "POSTGRES_DB" :: Either String String)
      <*> (parseVar' "POSTGRES_PASSWORD" :: Either String String)
      <*> (parseVar' "AUTH_KEY" :: Either String String)

  case res of
    (Right config) -> pure config
    (Left err) -> error ("Failed to parse config. " <> err)

getConfig :: IO Config
getConfig = do
  envVars <- getEnvironment
  let isProd = fromRight False $ (parseVar envVars "PROD" :: Either String Bool)
    
  if isProd then getEnvVarsConfig else getEnvFileConfig
