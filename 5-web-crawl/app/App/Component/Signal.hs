{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE NoImplicitPrelude #-}
module App.Component.Signal where

import RIO
import Control.Monad.Component (ComponentM, buildComponent_)
import System.Posix.Signals

addINTSignal :: ComponentM ()
addINTSignal = buildComponent_ "unix-sigINT-handler" $ do
  intSignal <- newEmptyMVar
  _prevHandler <- installHandler sigINT (Catch (putMVar intSignal ())) Nothing
  failingAsync <- async (takeMVar intSignal >> throwIO (ExitFailure (fromIntegral sigINT)))
  link failingAsync
