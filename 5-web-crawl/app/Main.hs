{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main (main) where

import RIO

import App.Component (appComponent)
import Control.Monad.Component (runComponentM)

import Types

run :: RIO App ()
run = do
  logInfo "In Run module"
  forever $ do
    withRemoteQueue "queue1" $ \queue -> do
      messages <- liftIO $ receiveMessages queue
      if null messages then
        logInfo "No messages found"
      else do
        mapM_ (logInfo . display) messages
        mapM_ (liftIO . deleteRemoteMsg) messages
    -- delay for 30 seconds
    threadDelay 5000100

main :: IO ()
main =
  runComponentM
    "stage-zero"
    appComponent
    (\app -> runRIO app run)
