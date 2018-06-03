{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module App.Component.AWS
  ( AWS.Service
  , buildAwsRemoteQueues
  , buildAwsRemoteTopics
  )
  where

import RIO
import qualified RIO.HashMap as HashMap
import qualified RIO.Text as Text

import Control.Exception (ErrorCall(..))

import qualified System.Etc as Etc

import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)

import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON (Parser, typeMismatch)

import qualified Data.Scientific as Scientific

import qualified Toxiproxy as Proxy
import qualified Network.URI as Net
import qualified Network.AWS as AWS
import qualified Network.AWS.SQS as AWS
import Servant.Client (ClientM)

import Control.Monad.Component (ComponentM, buildComponent_, buildComponent)

import App.Component.AWS.Env (buildEnv)
import App.Component.AWS.SQS (buildQueues)
import App.Component.AWS.SNS (buildTopics)
import App.Toxic (ToxiproxyInfo, parseToxiproxyInfo, toxiproxyUrl, buildToxiproxyFromURL)

import Types

--------------------------------------------------------------------------------

buildAwsRemoteQueues configRef logFn = do
  config <- readIORef configRef
  buildQueues config logFn

buildAwsRemoteTopics configRef logFn = do
  config <- readIORef configRef
  buildTopics config logFn
