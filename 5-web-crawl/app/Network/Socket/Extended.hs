{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Network.Socket.Extended where

import RIO
import qualified Network.Socket as Socket

getFreePort :: IO Integer
getFreePort = do
  bracket
    (do sock <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
        Socket.setSocketOption sock Socket.ReuseAddr 1
        Socket.setSocketOption sock Socket.ReusePort 1
        Socket.bind sock (Socket.SockAddrInet Socket.aNY_PORT Socket.iNADDR_ANY)
        return sock)
    Socket.close
    (\sock -> toInteger <$> Socket.socketPort sock)
