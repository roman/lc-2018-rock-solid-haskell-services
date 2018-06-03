{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module App.Toxic
       ( ToxiproxyInfo (..)
       , toxiproxyUrl
       , buildToxiproxyFromURL
       , parseToxiproxyInfo
       )
       where

import RIO
import qualified RIO.Text as Text

import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson.Types as JSON

import qualified Toxiproxy as Proxy
import qualified Network.Socket.Extended as Network
import qualified Network.URI as URI

data ToxiproxyInfo
  = ToxiproxyInfo
  {
    toxiproxyName      :: !Text
  , toxiproxyListen    :: !(Text, Maybe Integer)
  , toxiproxyToxicList :: ![Proxy.Toxic]
  }

parseToxiproxyInfo :: Text -> JSON.Value -> JSON.Parser ToxiproxyInfo
parseToxiproxyInfo name =
  JSON.withObject "ToxproxyInfo" $ \object -> do
     ToxiproxyInfo <$> pure name
                   <*> parseListen object
                   <*> (fromMaybe [] <$> (object .:? "toxins"))
  where
    parseListen object = (,) <$> (object .: "host") <*> (object .:? "port")

buildProxyListen :: MonadIO m => ToxiproxyInfo -> m Text
buildProxyListen ToxiproxyInfo {toxiproxyListen = (host, mport)} = do
  port <- maybe (liftIO Network.getFreePort) return mport
  return $ host <> ":" <> tshow port

toToxiproxyUpstream :: Text -> Maybe Text
toToxiproxyUpstream url = do
  uri  <- URI.parseURI (Text.unpack url)
  auth <- URI.uriAuthority uri
  return $ Text.pack $ URI.uriRegName auth <> URI.uriPort auth

toxiproxyUrl :: Bool -> Proxy.Proxy -> Text
toxiproxyUrl isSecure =
  let
    scheme = if isSecure then "https://" else "http://"
  in
    (scheme <>) . Proxy.proxyListen

buildToxiproxyFromURL :: MonadIO m => Text -> ToxiproxyInfo -> m (Maybe Proxy.Proxy)
buildToxiproxyFromURL upstreamUrl proxyInfo =
  case toToxiproxyUpstream upstreamUrl of
    Nothing -> return Nothing
    Just proxyUpstream -> do
      let
      proxyListen <- buildProxyListen proxyInfo
      return
        $ Just
        $ Proxy.Proxy {
              Proxy.proxyListen
            , Proxy.proxyUpstream
            , Proxy.proxyToxics   = toxiproxyToxicList proxyInfo
            , Proxy.proxyName     = fromString (Text.unpack $ toxiproxyName proxyInfo)
            , Proxy.proxyEnabled  = True
            }
