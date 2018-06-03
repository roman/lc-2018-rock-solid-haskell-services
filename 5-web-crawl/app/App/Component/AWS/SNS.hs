{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module App.Component.AWS.SNS (buildTopics) where

import RIO
import qualified RIO.HashMap as HashMap

import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as JSON

import qualified Network.AWS as AWS
import qualified Network.AWS.SNS as AWS

import qualified System.Etc as Etc

import Control.Monad.Component (ComponentM, buildComponent_, buildComponent)
import App.Component.AWS.Env (buildEnv, fetchSnsService)

import Types

buildSnsRemoteTopic :: AWS.HasEnv env => env -> Text -> RemoteTopic
buildSnsRemoteTopic env topicArn = do
    RemoteTopic { _publishMessage }
  where
    _publishMessage content =
      void
      $ AWS.runResourceT
      $ AWS.runAWS env
      $ AWS.send (AWS.publish content & set AWS.pTopicARN (Just topicArn))

data RemoteTopicEntry
  = RemoteTopicEntry
    {
      topicName :: !Text
    , topicArn :: !Text
    }

instance JSON.FromJSON RemoteTopicEntry where
  parseJSON = JSON.withObject "RemoteTopicEntry" $ \obj ->
    RemoteTopicEntry <$> obj .: "name"
                     <*> obj .: "arn"

buildTopics
  :: Etc.IConfig config
  => config
  -> LogFunc
  -> ComponentM (HashMap Text RemoteTopic)
buildTopics config logFn = do
  env <- buildEnv config logFn (fetchSnsService config)
  case Etc.getConfigValue ["aws", "sns", "topics"] config of
    Left err -> do
      runRIO logFn $ logWarn $ "No SNS topics found: " <> displayShow err
      return $ HashMap.empty

    Right topicArnList -> do
      topicList <- forM topicArnList $ \(RemoteTopicEntry {topicName, topicArn}) -> do
            runRIO logFn $ do
              logDebug
                $ "Registering topic " <> displayShow topicName
                <> " with ARN " <> displayShow topicArn

            remoteTopic <- buildComponent_ topicName $ return $ buildSnsRemoteTopic env topicArn
            return (topicName, remoteTopic)

      return $ HashMap.fromList topicList
