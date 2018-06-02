{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Types where


import RIO
import qualified RIO.HashMap as HashMap
-- import qualified Network.AWS.SQS as AWS

data App
  = App {
    appQueues    :: !(HashMap Text RemoteQueue)
  , appLogFunc   :: !LogFunc
  }

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
    receiveMessages :: !(IO [ RemoteMessage ])
  }

class HasRemoteQueue env where
  remoteQueueL :: Lens' env (HashMap Text RemoteQueue)

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\app logFunc -> app { appLogFunc = logFunc })

instance HasRemoteQueue App where
  remoteQueueL = lens appQueues (\app queues -> app { appQueues = queues })

withRemoteQueue
  :: (HasLogFunc env, HasRemoteQueue env)
  => Text -> (RemoteQueue -> RIO env ())
  -> RIO env ()
withRemoteQueue name callback = do
  remoteQueues <- view remoteQueueL <$> ask
  case HashMap.lookup name remoteQueues of
    Nothing -> logWarn $ "Did not find queue with name " <> displayShow name
    Just queue -> callback queue

--------------------------------------------------------------------------------
