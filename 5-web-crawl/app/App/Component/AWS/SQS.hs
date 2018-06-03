{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module App.Component.AWS.SQS where

import RIO
import qualified RIO.HashMap as HashMap

import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as JSON

import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)

import qualified Network.AWS as AWS
import qualified Network.AWS.SQS as AWS

import qualified System.Etc as Etc
import Control.Monad.Component (ComponentM, buildComponent_, buildComponent)

import Servant.Client (ClientM)
import qualified Toxiproxy as Proxy
import App.Toxic (ToxiproxyInfo, parseToxiproxyInfo, toxiproxyUrl, buildToxiproxyFromURL)

import App.Component.AWS.Util (withEndpointUrl)
import App.Component.AWS.Env (buildEnv, fetchSqsService)

import App.CircuitBreaker (
    CircuitBreakerState
  , defaultCircuitBreakerOptions
  , circuitBreaker
  )

import Types

--------------------------------------------------------------------------------


buildSqsQueue :: MonadUnliftIO m => AWS.Env -> Text -> m (CircuitBreakerState, RemoteQueue)
buildSqsQueue env queueUrl = do
    (status, receiveMessages) <-
      circuitBreaker defaultCircuitBreakerOptions receiveMessages_
    return (status, RemoteQueue { receiveMessages })
  where
    deleteMessage receipt = do
      void $ AWS.send $ AWS.deleteMessage queueUrl receipt

    toRemoteMessage msg = do
      body      <- msg ^. AWS.mBody
      receipt   <- msg ^. AWS.mReceiptHandle
      deleteMsg <- pure $ AWS.runResourceT $ AWS.runAWS env $ deleteMessage receipt
      return (RemoteMessage body deleteMsg)

    receiveMessages_ amount = AWS.runResourceT $ AWS.runAWS env $ do
        let
          req =
            AWS.receiveMessage queueUrl
            & set AWS.rmMaxNumberOfMessages (Just (min (max amount 1) 10))

        resp <- AWS.send req
        let messages = resp ^. AWS.rmrsMessages
        return $ mapMaybe toRemoteMessage messages

buildProxyAndToxics :: Proxy.Proxy -> ClientM ()
buildProxyAndToxics proxy = do
  void $ Proxy.createProxy proxy
  forM_ (Proxy.proxyToxics proxy) $ \toxic ->
    Proxy.createToxic (Proxy.proxyName proxy) toxic

teardownProxy :: Proxy.Proxy -> ClientM ()
teardownProxy proxy =
  void $ Proxy.deleteProxy (Proxy.proxyName proxy)

buildToxicQueue
  :: Proxy.Proxy
  -> Text
  -> AWS.Env
  -> ComponentM RemoteQueue
buildToxicQueue proxy queueUrl env = do
    let proxyUrl = toxiproxyUrl False proxy
    buildComponent componentName
      (Proxy.run (buildProxyAndToxics proxy)
        >>= either throwIO (const $ return ()))
      (\_  ->
          Proxy.run (teardownProxy proxy)
          >>= either throwIO (const $ return ()))

    withEndpointUrl proxyUrl env $ \toxicEnv ->
      liftIO (snd <$> buildSqsQueue toxicEnv queueUrl)
  where
    (Proxy.ProxyName componentName) = Proxy.proxyName proxy

--------------------------------------------------------------------------------

data RemoteQueueEntry = RemoteQueueEntry
  {
    queueUrl       :: !Text
  , queueName      :: !Text
  , queueToxicInfo :: !(Maybe ToxiproxyInfo)
  }

instance JSON.FromJSON RemoteQueueEntry where
  parseJSON = JSON.withObject "RemoteQueueEntry" $ \obj -> do
    queueName <- obj .: "name"
    mtoxic    <- obj .:? "toxic"
    case mtoxic of
      Nothing ->
        RemoteQueueEntry <$> obj .: "url"
                         <*> pure queueName
                         <*> pure Nothing
      Just toxicJson -> do
        let proxyName = queueName <> ".proxy"
        toxicInfo <- parseToxiproxyInfo proxyName toxicJson
        RemoteQueueEntry <$> obj .: "url"
                         <*> pure queueName
                         <*> pure (Just toxicInfo)

--------------------------------------------------------------------------------

buildQueues
  :: Etc.IConfig config
  => config -> LogFunc ->  ComponentM (HashMap Text RemoteQueue)
buildQueues config logFn = do
  env <- buildEnv config logFn (fetchSqsService config)
  case Etc.getConfigValue ["aws", "sqs", "queues"] config of
    Left err -> do
      runRIO logFn $ logWarn $ "No SQS Queues queue found: " <> displayShow err
      return $ HashMap.empty

    Right queueUrlList -> do
      queueList <- forM queueUrlList $ \(RemoteQueueEntry {queueUrl, queueName, queueToxicInfo}) -> do
        result <- runMaybeT $ do
          toxicInfo <- MaybeT $ return queueToxicInfo
          MaybeT $ buildToxiproxyFromURL queueUrl toxicInfo

        case result of
          Nothing -> do
            runRIO logFn $ do
              logDebug
                $ "Registering queue " <> displayShow queueName
                <> " with url " <> displayShow queueUrl

            queue <- buildComponent_ queueName (snd <$> buildSqsQueue env queueUrl)
            return (queueName, queue)

          Just proxyInfo -> do
            runRIO logFn
              $ logDebug
              $ "Registering queue " <> displayShow queueName
              <> " with url " <> displayShow queueUrl
              <> " and proxy " <> displayShow proxyInfo
              <> " (toxics included)"
            queue <- buildToxicQueue proxyInfo queueUrl env
            return (queueName, queue)

      return $ HashMap.fromList queueList
