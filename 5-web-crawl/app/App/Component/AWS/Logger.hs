{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module App.Component.AWS.Logger (toAWSLogger) where

import RIO
import qualified Control.Monad.Trans.AWS as AWS

toAWSLogger :: HasCallStack => LogFunc -> (AWS.LogLevel -> Builder -> IO ())
toAWSLogger logFunc = \logLevel builder ->
  flip runReaderT logFunc $ do
    logGeneric "" (transLogLevel logLevel) (Utf8Builder builder)

transLogLevel :: AWS.LogLevel -> LogLevel
transLogLevel logLevel =
  case logLevel of
    AWS.Info  -> LevelInfo
    AWS.Error -> LevelError
    AWS.Debug -> LevelDebug
    AWS.Trace -> LevelDebug
