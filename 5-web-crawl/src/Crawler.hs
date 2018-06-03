{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Crawler (processUrlWorker) where

import RIO

import qualified RIO.Set as Set
import qualified RIO.Text as Text
import qualified RIO.ByteString.Lazy as LB

import Network.URI (uriScheme, uriAuthority, uriAuthority, uriRegName)
import Network.HTTP.Client (Manager, Request, parseRequest, httpLbs, responseBody, getUri)
import Network.HTTP.Client.TLS (newTlsManagerWith, tlsManagerSettings)
import Text.XML.HXT.Core ((>>>))
import Text.HandsomeSoup ((!), parseHtml, css)

import qualified Text.XML.HXT.Core as HXT

inferUrlDomain :: Request -> String -> Maybe Text
inferUrlDomain req linkStr =
  let
    link = Text.pack linkStr
    uri = getUri req
  in
    if Text.isPrefixOf "mailto:" link then
      Nothing

    else if Text.isPrefixOf "//" link then
      Just $ Text.pack (uriScheme uri) <> link

    else if Text.isPrefixOf "/" link || Text.isPrefixOf "./" link  then
      case uriRegName <$> uriAuthority uri of
        Just domain ->
          Just
            $ Text.pack (uriScheme uri)
            <> "//"
            <> Text.pack domain
            <> link  -- Use domain from the req

        Nothing ->
          Nothing

    else if Text.isPrefixOf "http://" link || Text.isPrefixOf "https://" link then
      Just link

    else
      Nothing

fetchUrlList
  :: (HasLogFunc env, MonadReader env m, MonadIO m)
  => Manager -> Text -> m (Either UnicodeException [Text])
fetchUrlList manager url =  do
  request  <- liftIO $ parseRequest (Text.unpack url)
  -- logInfo $ "Perform request to URL: " <> display url
  response <- liftIO $ httpLbs request manager
  -- logInfo $ "Response accquired:"
  -- logInfo $ "  status: " <> display (statusCode $ responseStatus response)
  -- logInfo $ "  length: " <> display (LB.length $ responseBody response)
  -- logInfo $ displayShow (responseBody response)
  let edoc = (parseHtml . Text.unpack) <$> (decodeUtf8' $ LB.toStrict (responseBody response))
  case edoc of
    Left err ->
      return $ Left err
    Right doc -> do
      links <- liftIO $ HXT.runX (doc >>> css "a" ! "href")
      -- logInfo $ "Links accquired: " <> display (length links)
      return $ Right $ catMaybes $ map (inferUrlDomain request) links

processUrlWorker
  :: (Show identifier, HasLogFunc env, MonadIO m, MonadReader env m)
  => Manager
  -> IORef (Set Text)
  -> (Text -> m ())
  -> identifier
  -> Text
  -> m ()
processUrlWorker manager visitedUrlsRef addLink workerId url = do
  shouldContinue <- atomicModifyIORef' visitedUrlsRef $ \urlSet ->
    if Set.member url urlSet then
      (urlSet, False)
    else
      (Set.insert url urlSet, True)

  if shouldContinue then do
    logInfo $ "Worker " <> displayShow workerId <> " processing " <> display url
    elinks <- fetchUrlList manager url
    case elinks of
      Left err ->
        logError $ "Worker receiveid a response with invalid encoding " <> displayShow err
      Right links -> do
        -- One URL at a time to not hog the manager; in an ideal world, addLink
        -- would store the link in a persistent layer
        logError $ "ROMAN HERE, check number of links: " <> display (length links)
        mapM_ addLink links
  else
    logInfo $ "Worker " <> displayShow workerId <> " ignoring " <> display url
