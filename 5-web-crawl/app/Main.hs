{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main (main) where

import RIO

import App.Component (appComponent)
import Control.Monad.Component (runComponentM)

main :: IO ()
main =
  runComponentM
    "stage-zero"
    appComponent
    (\app -> runRIO app (return ()))
