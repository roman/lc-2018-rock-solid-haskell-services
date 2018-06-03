{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main (main) where

import RIO
import Capataz

import App.Component (buildAppComponent)
import Control.Monad.Component (runComponentM)

import Types

run :: RIO App ()
run = do
  logInfo "In Run module"
  streamQueue "queue1" 5 1000100 $ \items -> do
    mapM_ (logInfo . display) items
    mapM_ (liftIO . deleteRemoteMsg) items

main :: IO ()
main =
  runComponentM
    "web-crawler"
    buildAppComponent
    (\(app, capataz) -> runRIO app (joinCapatazThread capataz))
