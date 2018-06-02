{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import RIO
import RIO.Orphans ()
import qualified RIO.Text as Text

import Data.FileEmbed (embedFile)
import qualified System.Etc as Etc

import Data.Aeson ((.:))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON (Parser)

import Data.Pool (Pool, destroyAllResources)
import Database.Persist.Sql (runMigration, runSqlPool)
import Database.Persist.Postgresql (SqlBackend, ConnectionString, createPostgresqlPool)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Control.Monad.Logger (MonadLogger)

import Control.Monad.Component (ComponentM, buildComponent, buildComponent_, runComponentM)
import Control.Monad.Component.Development (runComponentDevel)

--------------------------------------------------------------------------------
-- Data Structures

data SimpleApp =
  SimpleApp {
      appLogFunc :: !LogFunc
    , appDbPool  :: !(Pool SqlBackend)
    }

instance HasLogFunc SimpleApp where
  logFuncL = lens appLogFunc $ \x y -> x { appLogFunc = y }

--------------------------------------------------------------------------------
-- Configuration

specBytes :: ByteString
specBytes =
  $(embedFile "./config/spec.yaml")

parseConfigSpec :: MonadThrow m => m (Etc.ConfigSpec ())
parseConfigSpec =
  case Text.decodeUtf8' specBytes of
    Left err -> throwM err
    Right result -> Etc.parseConfigSpec result

resolveConfigSpec :: Etc.ConfigSpec () -> IO (Etc.Config, Vector SomeException)
resolveConfigSpec configSpec = do
  let
    defaultConfig = Etc.resolveDefault configSpec

  (fileConfig, fileWarnings) <- Etc.resolveFiles configSpec
  envConfig <- Etc.resolveEnv configSpec
  cliConfig <- Etc.resolvePlainCli configSpec

  return ( defaultConfig <> fileConfig <> envConfig <> cliConfig
         , fileWarnings
         )

buildConfig :: ComponentM (Etc.Config, Vector SomeException)
buildConfig = buildComponent_ "config" $ do
  configSpec <- parseConfigSpec
  resolveConfigSpec configSpec

--------------------------------------------------------------------------------
-- Database

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
   Person
     name String
     age Int Maybe
     deriving Show
  |]

parseConnString :: JSON.Value -> JSON.Parser ByteString
parseConnString = JSON.withObject "ConnString" $ \obj -> do
  user <- obj .: "username"
  password <- obj .: "password"
  database <- obj .: "database"
  return $
    Text.encodeUtf8
    $ Text.unwords [ "user=" <> user
                   , "password=" <> password
                   , "dbname=" <> database
                   ]

buildDatabasePool :: Etc.Config -> LogFunc -> ComponentM (Pool SqlBackend)
buildDatabasePool config logFunc = do
  connString <- Etc.getConfigValueWith parseConnString ["database"] config
  buildComponent "database-pool"
    (runRIO logFunc $ createPostgresqlPool connString 1)
    destroyAllResources

runMigrations :: LogFunc -> Pool SqlBackend -> ComponentM ()
runMigrations logFunc pool =
  buildComponent_ "database-migrations" $ runSqlPool (runMigration migrateAll) pool

--------------------------------------------------------------------------------
-- Logging

parseLogHandle :: JSON.Value -> JSON.Parser Handle
parseLogHandle = JSON.withText "IOHandle" $ \_handleText ->
  -- TODO: Make sure we parse the text and return the correct handle
  return stdout

buildLogOptions :: Etc.Config -> IO LogOptions
buildLogOptions _config = do
  -- TODO: Get logging out of the config, make sure you use parseLogHandle
  logOptionsHandle stdout True

buildLogger :: Etc.Config -> ComponentM LogFunc
buildLogger config = do
  logOptions       <- liftIO $ buildLogOptions config
  (appLogFunc, _)  <- buildComponent "logger" (newLogFunc logOptions) snd
  return appLogFunc

--------------------------------------------------------------------------------
-- Build Application

buildApplication :: ComponentM SimpleApp
buildApplication = do
  (config, fileWarnings) <- buildConfig
  appLogFunc     <- buildLogger config
  appDbPool      <- buildDatabasePool config appLogFunc

  renderedConfig <- Etc.renderConfig config
  liftIO
    $ runRIO appLogFunc $ do
      mapM_ (logWarn . displayShow) fileWarnings
      logInfo $ "Configuration\n" <> displayShow renderedConfig

  return SimpleApp { appLogFunc, appDbPool = undefined }

--------------------------------------------------------------------------------
-- Main

main :: IO ()
main =
  runComponentM
    "component-program"
    buildApplication $ \app ->
      runRIO app $ return ()

develMain :: IO ()
develMain =
  runComponentDevel
    (traceDisplayIO . display)
    "component-program"
    buildApplication $ \app ->
      runRIO app $ traceIO "Hello World"
