{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module App.Component.AWS.Util where

import RIO
import qualified RIO.Text as Text

import Control.Exception (ErrorCall(..))

import qualified Network.AWS as AWS
import qualified Network.URI as Net

urlToEndpoint :: Text -> Maybe AWS.Endpoint
urlToEndpoint url = do
    uri       <- Net.parseURI (Text.unpack url)
    authority <- Net.uriAuthority uri
    port      <- safeTail (Net.uriPort authority) >>= readMaybe
    let
      host = Text.encodeUtf8 . Text.pack . Net.uriRegName $ authority
      isSecure = Net.uriScheme uri == "https:"

    -- TODO: Need to properly work out the region parameter
    return $ AWS.Endpoint host isSecure port "us-east-1"
  where
    safeTail input =
      case input of
        [] -> Nothing
        (_:output) -> Just output

withEndpointUrl :: (AWS.HasEnv env, MonadThrow m) => Text -> env -> (env -> m a) -> m a
withEndpointUrl url env f =
  case urlToEndpoint url of
    Nothing ->
      throwM (ErrorCall $ "Invalid URL given: " <> show url)
    Just endpoint ->
      let env1 = AWS.override (& set AWS.serviceEndpoint endpoint) env
      in f env1
