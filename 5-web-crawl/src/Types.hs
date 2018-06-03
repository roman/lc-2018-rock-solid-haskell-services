{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Types where


import RIO
import qualified RIO.HashMap as HashMap

--------------------------------------------------------------------------------

data RemoteMessage
  = RemoteMessage {
    remoteMsgPayload :: !Text
  , deleteRemoteMsg  :: !(IO ())
  }

instance Display RemoteMessage where
  display (RemoteMessage {remoteMsgPayload}) =
    display remoteMsgPayload

data RemoteQueue
  = RemoteQueue {
    receiveMessages :: !(Int -> IO [ RemoteMessage ])
  }

class HasRemoteQueue env where
  remoteQueueL :: Lens' env (HashMap Text RemoteQueue)

withRemoteQueue
  :: (HasLogFunc env, HasRemoteQueue env, MonadReader env m, MonadIO m)
  => Text -> (RemoteQueue -> m ())
  -> m ()
withRemoteQueue name callback = do
  remoteQueues <- view remoteQueueL <$> ask
  case HashMap.lookup name remoteQueues of
    Nothing -> logWarn $ "Did not find queue with name " <> displayShow name
    Just queue -> callback queue

streamQueue
  :: (HasRemoteQueue env, HasLogFunc env, MonadReader env m, MonadIO m)
  => Text -> Int -> Int -> ([RemoteMessage] -> m ()) -> m ()
streamQueue name itemsPerCall requestRateMicros callbackFn =
  withRemoteQueue name $ \queue ->
    forever $ do
      result <- liftIO $ receiveMessages queue itemsPerCall
      unless (null result) (callbackFn result)
      threadDelay requestRateMicros

--------------------------------------------------------------------------------

data RemoteTopic
  = RemoteTopic {
      _publishMessage :: Text -> IO ()
    }

class HasRemoteTopic env where
  remoteTopicL :: Lens' env (HashMap Text RemoteTopic)

publishMessage
  :: (HasLogFunc env, HasRemoteTopic env, MonadReader env m, MonadIO m)
  => Text
  -> Text
  -> m ()
publishMessage topicName contents = do
  topicMap <- view remoteTopicL
  case HashMap.lookup topicName topicMap  of
    Nothing -> logWarn $ "Specified topic not present " <> display topicName
    Just remoteTopic -> liftIO $ _publishMessage remoteTopic contents

--------------------------------------------------------------------------------

data App
  = App {
    appQueues    :: !(HashMap Text RemoteQueue)
  , appTopics    :: !(HashMap Text RemoteTopic)
  , appLogFunc   :: !LogFunc
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\app logFunc -> app { appLogFunc = logFunc })

instance HasRemoteQueue App where
  remoteQueueL = lens appQueues (\app queues -> app { appQueues = queues })

instance HasRemoteTopic App where
  remoteTopicL = lens appTopics (\app topics -> app { appTopics = topics })

--------------------------------------------------------------------------------
