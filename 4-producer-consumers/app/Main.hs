{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import RIO
import Capataz

import qualified RIO.Text as Text

writerSpec
  :: (HasLogFunc env, MonadReader env m, MonadIO m)
  => TQueue (Either Text Int)
  -> Int
  ->  ProcessSpec m
writerSpec queue index = do
    workerSpec ("writer-" <> tshow index) (runWriter 0) (set workerRestartStrategyL Transient)
  where
    runWriter = fix $ \f n -> do
      if n `mod` 5 == 0 then do
        logInfo "send failure"
        atomically $ writeTQueue queue (Left "fail")
      else do
        logInfo $ "send number " <> display n
        atomically $ writeTQueue queue (Right n)

      threadDelay 1000100
      f (n + 1)

readerSpec
  :: (HasLogFunc env, MonadReader env m, MonadThrow m, MonadIO m)
  => TQueue (Either Text Int) -> Int ->  ProcessSpec m
readerSpec queue index = do
    workerSpec ("reader-" <> tshow index) (runReader 0) (set workerRestartStrategyL Temporary)
  where
    runReader = fix $ \f n -> do
      result <- atomically $ readTQueue queue
      case result of
        Left err -> do
          logInfo "Failing from message"
          error (Text.unpack err)
        Right n -> logInfo $ "Received " <> display n
      threadDelay 1000100
      f (n + 1)

main :: IO ()
main = do
  logOptions <- logOptionsHandle stdout True
  withLogFunc logOptions $ \logFunc -> runRIO logFunc $ do
    queue <- newTQueueIO
    bracket (forkCapataz "producer-consumer"
                         (set onSystemEventL (logOther "capataz" . display)))
            terminateCapataz_ $ \capataz -> do

      _writerSupervisor <-
        forkSupervisor
          (buildSupervisorOptions "writers"
                                  (set supervisorProcessSpecListL (map (writerSpec queue) [1..10])))
          capataz

      _readerSupervisor <-
        forkSupervisor
          (buildSupervisorOptions "readers"
                                  (set supervisorProcessSpecListL (map (readerSpec queue) [1..10])))
          capataz

      joinCapatazThread capataz
