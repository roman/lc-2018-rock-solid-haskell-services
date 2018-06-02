{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module App.Component.Config (fetchConfig) where

import RIO
import qualified RIO.Text as Text

import Data.FileEmbed (embedFile)
import qualified System.Etc as Etc

import Control.Monad.Component (ComponentM, buildComponent_)

configBytes :: ByteString
configBytes =
  $(embedFile "./config/spec.yaml")

readConfigSpec :: Either SomeException (Etc.ConfigSpec ())
readConfigSpec = do
   configTxt <- mapLeft toException $ Text.decodeUtf8' configBytes
   Etc.parseConfigSpec configTxt

loadConfig :: Etc.ConfigSpec cmd -> IO (Etc.Config, Vector SomeException)
loadConfig configSpec = do
  let defConfig = Etc.resolveDefault configSpec
  envConfig <- Etc.resolveEnv configSpec
  (fileConfig, warnings) <- Etc.resolveFiles configSpec
  return $ (defConfig <> envConfig <> fileConfig, warnings)

fetchConfig :: ComponentM ( IORef Etc.Config
                          , Vector SomeException
                          )
fetchConfig = buildComponent_ "config" $ do
  case readConfigSpec of
    Left err ->
      throwIO err

    Right configSpec -> do
      (config, warnings) <- loadConfig configSpec
      configRef          <- newIORef config
      return (configRef, warnings)
