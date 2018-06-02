{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module App.Component.Logger (buildLogFunc) where

import RIO
import qualified RIO.Text as Text

import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON (typeMismatch)
import qualified System.Etc as Etc

import Control.Monad.Component (ComponentM, buildComponent)

fetchIoHandle :: (MonadThrow m, Etc.IConfig config) => config -> m Handle
fetchIoHandle config =
    Etc.getConfigValueWith parseHandle ["logger", "handle"] config
  where
    parseHandle = JSON.withText "IO.Handle" $ \handleName ->
      case Text.toLower handleName of
        "stdout" -> return stdout
        "stderr" -> return stderr
        _ -> JSON.typeMismatch "IO.Handle" (JSON.String handleName)

fetchLogLevel :: (MonadThrow m, Etc.IConfig config) => config -> m LogLevel
fetchLogLevel config =
    Etc.getConfigValueWith parseLogLevel ["logger", "level"] config
  where
    parseLogLevel = JSON.withText "LogLevel" $ \levelName ->
      return $ case Text.toLower levelName of
        "debug" -> LevelDebug
        "info" -> LevelInfo
        "warn" -> LevelWarn
        "error" -> LevelError
        _ -> LevelOther levelName

buildLogOptions :: (MonadIO m, Etc.IConfig config, MonadThrow m) => config -> m LogOptions
buildLogOptions config = do
  handle'     <- fetchIoHandle config
  level       <- fetchLogLevel config
  verbose     <- Etc.getConfigValue ["logger", "verbose"] config
  logOptions  <- logOptionsHandle handle' verbose
  return $ logOptions & setLogMinLevel level

buildLogFunc :: IORef Etc.Config -> ComponentM LogFunc
buildLogFunc configRef = do
  config <- readIORef configRef
  logOptions <- buildLogOptions config
  fst <$> buildComponent "logger" (newLogFunc logOptions) snd
