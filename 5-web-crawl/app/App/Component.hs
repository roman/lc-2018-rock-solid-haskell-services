{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module App.Component where

import RIO

import qualified System.Etc as Etc

import qualified App.Component.Config as Config
import qualified App.Component.Logger as Logger
import qualified App.Component.AWS as AWS
import qualified App.Component.Signal as Signal

import Control.Monad.Component (ComponentM)
import Types

appComponent :: ComponentM App
appComponent = do
  (configRef, warnings) <- Config.fetchConfig
  logFunc <- Logger.buildLogFunc configRef

  -- DEBUG configuration
  runRIO logFunc $ do
    unless (null warnings) $
      mapM_ (logWarn . displayShow) warnings

    configDoc <- readIORef configRef >>= Etc.renderConfig
    logInfo $ "\n# Application Configuration\n\n" <> displayShow configDoc

  Signal.addINTSignal
  App <$> AWS.buildAwsRemoteQueues configRef logFunc
      <*> pure logFunc
