{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module App.Component where

import RIO
import Capataz

import qualified System.Etc as Etc

import qualified App.Component.Config as Config
import qualified App.Component.Logger as Logger
import qualified App.Component.AWS as AWS
import qualified App.Component.Signal as Signal
import qualified App.Component.Crawler as Crawler

import Control.Monad.Component (ComponentM)
import Types

buildAppComponent :: ComponentM (App, Capataz (RIO App))
buildAppComponent = do
  Signal.addINTSignal

  (configRef, warnings) <- Config.fetchConfig
  logFunc <- Logger.buildLogFunc configRef

  -- DEBUG configuration
  runRIO logFunc $ do
    unless (null warnings) $
      mapM_ (logWarn . displayShow) warnings

    configDoc <- readIORef configRef >>= Etc.renderConfig
    logInfo $ "\n# Application Configuration\n\n" <> displayShow configDoc

  app <-
      App <$> AWS.buildAwsRemoteQueues configRef logFunc
          <*> AWS.buildAwsRemoteTopics configRef logFunc
          <*> pure logFunc

  capataz <- Crawler.buildWebCrawler configRef app
  return (app, capataz)
