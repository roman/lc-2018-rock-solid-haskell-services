{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module App.Component.Crawler where

import RIO
import qualified RIO.Text as Text
import qualified RIO.Text.Lazy as Text (toStrict)
import qualified RIO.Set as Set

import Capataz

import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManagerWith, tlsManagerSettings)
import Control.Monad.Component (ComponentM, buildComponent)

import Data.Aeson ((.=))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Text as JSON (encodeToLazyText)
import Lens.Micro ((^?))
import Lens.Micro.Aeson (key, values, _String, _Value)

import qualified System.Etc as Etc

import Crawler
import Types

remoteQueueReader
  :: (HasRemoteQueue env, HasLogFunc env)
  => (RemoteMessage -> RIO env ())
  -> Text
  -> Int
  -> RIO env ()
remoteQueueReader sendToWorker urlQueueName workerCount = do
  let pullRateMicros = 1500100 -- 1.5 secs
  streamQueue urlQueueName workerCount pullRateMicros (mapM_ sendToWorker)

remoteQueueReaderSpec
  :: (HasRemoteQueue env, HasLogFunc env)
  => (RemoteMessage -> RIO env ())
  -> Text
  -> Int
  -> ProcessSpec (RIO env)
remoteQueueReaderSpec sendToWorker urlQueueName workerCount =
  -- sqs poller
  workerSpec "remote-queue-reader"
             (remoteQueueReader sendToWorker urlQueueName workerCount)
             (set workerRestartStrategyL Permanent)

getUrlFromMessage contents =
  contents ^? key "Message" . _String . key "url" . _String

toUrlMessage sendLink url = do
  let value =
        Text.toStrict
        $ JSON.encodeToLazyText
        $ JSON.object ["url" .= url]
  sendLink value

crawlerWorkerPoolSpec
  :: HasLogFunc env
  => Manager
  -> (RIO env RemoteMessage)
  -> (Text -> RIO env ())
  -> Int
  -> RIO env (ProcessSpec (RIO env))
crawlerWorkerPoolSpec manager pullWork pushLink workerCount = do
  visitedSetRef <- newIORef Set.empty
  buildStealWorkerPoolSpec
    WorkerPoolArgs {
        poolSupervisorName = "web-crawler-worker-supervisor"
      , poolSupervisorOptions = id
      , poolPullNewWork = pullWork
      , poolWorkerNamePrefix = "web-crawler-worker"
      , poolWorkerCount = workerCount
      , poolWorkerAction = \workerId msg -> do
          let body = remoteMsgPayload msg
          case getUrlFromMessage body of
            Nothing ->
              logWarn $ "invalid message received, skipping for now: " <> display body
            Just url -> do
              processUrlWorker manager visitedSetRef (toUrlMessage pushLink) workerId url
              liftIO $ deleteRemoteMsg msg
      , poolWorkerOptions = set workerRestartStrategyL Permanent
      }

buildWebCrawlerSpec
  :: (HasRemoteQueue env, HasRemoteTopic env, HasLogFunc env)
  => Etc.Config
  -> RIO env (ProcessSpec (RIO env))
buildWebCrawlerSpec config = do
  manager <- newTlsManagerWith tlsManagerSettings
  remoteQueueName <- Etc.getConfigValue ["crawler", "url-queue-name"] config
  remoteTopicName <- Etc.getConfigValue ["crawler", "url-topic-name"] config

  -- TODO: Get worker count from configuration
  let workerCount = 1
  workQueue <- newTBQueueIO workerCount
  let
    sendToWorker = atomically . writeTBQueue workQueue
    pullWork = atomically $ readTBQueue workQueue
    pushLink = publishMessage remoteTopicName

  workerPoolSpec <-
    crawlerWorkerPoolSpec manager pullWork pushLink workerCount

  return
    $ supervisorSpec "web-crawler-supervisor"
                 (set supervisorProcessSpecListL
                      [ remoteQueueReaderSpec sendToWorker remoteQueueName workerCount
                      , workerPoolSpec
                      ])

buildWebCrawler
  :: ( HasLogFunc env
     , HasRemoteTopic env
     , HasRemoteQueue env
     )
  => IORef Etc.Config
  -> env
  -> ComponentM (Capataz (RIO env))
buildWebCrawler configRef app = do
  config <- readIORef configRef
  buildComponent
    "supervision-tree"
    (runRIO app $ do
        crawlerSpec <- buildWebCrawlerSpec config
        forkCapataz "root"
          ( set supervisorProcessSpecListL [crawlerSpec]
          . set onSystemEventL (logOther "capataz" . display)
          ))
    (runRIO app . terminateCapataz_)
