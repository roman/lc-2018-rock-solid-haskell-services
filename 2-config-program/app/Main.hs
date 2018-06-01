{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import RIO
import qualified RIO.Text as Text

import Data.FileEmbed (embedFile)
import qualified System.Etc as Etc

--------------------------------------------------------------------------------
--

data SimpleApp =
  SimpleApp { appLogFunc  :: LogFunc
--            , appDatabase ::
            }

--------------------------------------------------------------------------------
-- Configuration

specBytes :: ByteString
specBytes =
  $(embedFile "./config/spec.yaml")

parseConfigSpec :: MonadThrow m => m (Etc.ConfigSpec ())
parseConfigSpec =
  case Text.decodeUtf8' specBytes of
    Left err -> throwM err
    Right result -> Etc.parseConfigSpec result

resolveConfigSpec :: MonadIO m => Etc.ConfigSpec () -> m (Etc.Config, Vector [SomeException])
resolveConfigSpec configSpec = do
  let
    defaultConfig = Etc.resolveDefault configSpec

  (fileConfig, fileWarnings) <- Etc.resolveFiles configSpec
  envConfig <- Etc.resolveEnv configSpec
  cliConfig <- Etc.resolvePlainCli configSpec

  return (defaultConfig <> fileConfig <> envConfig <> cliConfig, fileWarnings)

--------------------------------------------------------------------------------
-- Main

main :: IO ()
main = do
  configSpec <- parseConfigSpec
  (config, _fileWarnings) <- resolveConfigSpec configSpec
  logOptions <- logOptionsHandle stdout True
  withLogFunc logOptions $ \logFunc ->
    runRIO (SimpleApp logFunc) $ do
      return ()
