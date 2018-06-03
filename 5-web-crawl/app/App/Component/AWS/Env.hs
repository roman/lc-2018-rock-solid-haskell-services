{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module App.Component.AWS.Env
  ( buildEnv
  , fetchSqsService
  , fetchSnsService
  ) where

import RIO
import qualified RIO.Text as Text


import qualified Data.Scientific as Scientific
import qualified Network.URI as Net

import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON (Parser, typeMismatch)

import qualified Network.AWS as AWS
import qualified Network.AWS.SQS as AWS (sqs)
import qualified Network.AWS.SNS as AWS (sns)

import qualified System.Etc as Etc

import App.Component.AWS.Logger (toAWSLogger)
import App.Component.AWS.Util (urlToEndpoint)

--------------------------------------------------------------------------------


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

fetchSqsService :: Etc.IConfig config => config -> AWS.Service
fetchSqsService config =
    AWS.sqs
    & updateEndpoint (Etc.getConfigValueWith parseEndpoint ["aws", "sqs", "endpoint"] config)
    & updateTimeout  (Etc.getConfigValueWith parseTimeout  ["aws", "sqs", "timeout"] config)

fetchSnsService :: Etc.IConfig config => config -> AWS.Service
fetchSnsService config =
    AWS.sns
    & updateEndpoint (Etc.getConfigValueWith parseEndpoint ["aws", "sns", "endpoint"] config)
    & updateTimeout  (Etc.getConfigValueWith parseTimeout  ["aws", "sns", "timeout"] config)

fetchCredential :: Etc.IConfig config => config -> Maybe AWS.Credentials
fetchCredential config =
  AWS.FromKeys
    <$> Etc.getConfigValue ["aws", "access_key"] config
    <*> Etc.getConfigValue ["aws", "secret_key"] config

buildEnv :: (Etc.IConfig config, MonadIO m) => config -> LogFunc -> AWS.Service -> m AWS.Env
buildEnv config logFunc service = do
  let credential = fromMaybe AWS.Discover (fetchCredential config)

  env0 <- liftIO $ AWS.newEnv credential

  let
    env =
      AWS.override (const service)
      $ set AWS.envLogger (toAWSLogger logFunc) env0

  return env
