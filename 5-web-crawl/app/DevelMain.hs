{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module DevelMain where

import RIO

import App.Component (appComponent)
import Control.Monad.Component.Development (ComponentEvent(..), runComponentDevel)

main :: IO ()
main =
  runComponentDevel
    (traceDisplayIO . display)
    "stage-zero"
    appComponent
    (flip runRIO (return ()))
