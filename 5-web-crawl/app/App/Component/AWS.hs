{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module App.Component.AWS
  ( AWS.Service
  , buildAwsRemoteQueues
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

import App.Component.AWS.Logger (toAWSLogger)
import App.Toxic (ToxiproxyInfo, parseToxiproxyInfo, toxiproxyUrl, buildToxiproxyFromURL)

import Types

urlToEndpoint :: Text -> Maybe AWS.Endpoint
urlToEndpoint url = do
    uri       <- Net.parseURI (Text.unpack url)
    authority <- Net.uriAuthority uri
    port      <- safeTail (Net.uriPort authority) >>= readMaybe
    let
      host = Text.encodeUtf8 . Text.pack . Net.uriRegName $ authority
      isSecure = Net.uriScheme uri == "https:"

    -- TODO: Need to properly work out the region parameter
    return $ AWS.Endpoint host isSecure port "us-east-1"
  where
    safeTail input =
      case input of
        [] -> Nothing
        (_:output) -> Just output

parseEndpoint :: JSON.Value -> JSON.Parser AWS.Endpoint
parseEndpoint = JSON.withText "AWS.Endpoint" $ \urlTxt -> do
  maybe (JSON.typeMismatch "AWS.Endpoint" (JSON.String urlTxt))
        return
        (urlToEndpoint urlTxt)

parseTimeout :: JSON.Value -> JSON.Parser AWS.Seconds
parseTimeout = JSON.withScientific "AWS.Seconds" $ \number ->
  case Scientific.toBoundedInteger number of
    Nothing ->
      JSON.typeMismatch "AWS.Seconds" (JSON.Number number)
    Just seconds ->
      return $ AWS.Seconds seconds

updateEndpoint :: Maybe AWS.Endpoint -> AWS.Service -> AWS.Service
updateEndpoint mValue record =
  maybe record (\value -> set AWS.serviceEndpoint value record) mValue

updateTimeout :: Maybe AWS.Seconds -> AWS.Service -> AWS.Service
updateTimeout mValue record =
  maybe record (\value -> set AWS.serviceTimeout (Just value) record) mValue

withEndpointUrl :: (AWS.HasEnv env, MonadThrow m) => Text -> env -> (env -> m a) -> m a
withEndpointUrl url env f =
  case urlToEndpoint url of
    Nothing ->
      throwM (ErrorCall $ "Invalid URL given: " <> show url)
    Just endpoint ->
      let env1 = AWS.override (& set AWS.serviceEndpoint endpoint) env
      in f env1

fetchSqsService :: Etc.Config -> AWS.Service
fetchSqsService config =
    AWS.sqs
    & updateEndpoint (Etc.getConfigValueWith parseEndpoint ["aws", "sqs", "endpoint"] config)
    & updateTimeout  (Etc.getConfigValueWith parseTimeout  ["aws", "sqs", "timeout"] config)

fetchCredential :: Etc.Config -> Maybe AWS.Credentials
fetchCredential config =
  AWS.FromKeys
    <$> Etc.getConfigValue ["aws", "access_key"] config
    <*> Etc.getConfigValue ["aws", "secret_key"] config

buildEnv :: Etc.Config -> LogFunc -> RIO LogFunc AWS.Env
buildEnv config logFunc = do
  let sqsService = fetchSqsService config
      credential = fromMaybe AWS.Discover (fetchCredential config)

  env0 <- liftIO $ AWS.newEnv credential
  logInfo "AWS Environment created"

  let
    env =
      AWS.override (const sqsService)
      $ set AWS.envLogger (toAWSLogger logFunc) env0

  return env

buildSqsQueue :: AWS.Env -> Text -> RemoteQueue
buildSqsQueue env queueUrl = do
    RemoteQueue { receiveMessages }
  where
    deleteMessage receipt = do
      void $ AWS.send $ AWS.deleteMessage queueUrl receipt

    toRemoteMessage msg = do
      body      <- msg ^. AWS.mBody
      receipt   <- msg ^. AWS.mReceiptHandle
      deleteMsg <- pure $ AWS.runResourceT $ AWS.runAWS env $ deleteMessage receipt
      return (RemoteMessage body deleteMsg)

    receiveMessages = AWS.runResourceT $ AWS.runAWS env $ do
        response <- AWS.send $ AWS.receiveMessage queueUrl
        let messages = response ^. AWS.rmrsMessages
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
      return $ buildSqsQueue toxicEnv queueUrl
  where
    (Proxy.ProxyName componentName) = Proxy.proxyName proxy


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

buildQueues
  :: Etc.IConfig config
  => LogFunc -> config -> AWS.Env -> ComponentM (HashMap Text RemoteQueue)
buildQueues logFn config env = do
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

            queue <- buildComponent_ queueName $ return $ buildSqsQueue env queueUrl
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

buildAwsRemoteQueues :: IORef Etc.Config -> LogFunc -> ComponentM (HashMap Text RemoteQueue)
buildAwsRemoteQueues configRef logFn = do
  (config, env) <- buildComponent_ "aws" $ do
    config <- readIORef configRef
    env    <- runRIO logFn $ buildEnv config logFn
    return (config, env)
  buildQueues logFn config env

--------------------------------------------------------------------------------
