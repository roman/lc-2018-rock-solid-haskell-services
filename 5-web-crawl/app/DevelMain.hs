{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module DevelMain where

import RIO
import Capataz

import App.Component (buildAppComponent)
import Control.Monad.Component.Development (ComponentEvent(..), runComponentDevel)

main :: IO ()
main =
  runComponentDevel
    (traceDisplayIO . display)
    "stage-zero"
    buildAppComponent
    (\(app, capataz) -> runRIO app (joinCapatazThread capataz))
