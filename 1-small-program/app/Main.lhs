This file contains various main setups using the rio package.

Is always recommended to have at least these two extensions
in every file, @OverloadedStrings@ allows @Text@ type to be used
as double quotes, The @NoImplicitPrelude@ removes the import of the
default @Prelude@.


> {-# LANGUAGE NamedFieldPuns    #-}
> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE NoImplicitPrelude #-}
> module Main where
>
> import RIO

import RIO.Text
import RIO.ByteString
import RIO.Set
import RIO.HashMap
import RIO.Vector

> import RIO.Process (
>     HasProcessContext(..)
>   , ProcessContext
>   , mkDefaultProcessContext
>   , proc
>   , runProcess_
>   )

VERSION 1

> mainTrace :: IO ()
> mainTrace = do
>   traceIO "Hello World"


VERSION 2

> mainLogger :: IO ()
> mainLogger = do
>   logOptions <- logOptionsHandle stdout True
>   withLogFunc logOptions $ \logFunc ->
>     runRIO logFunc $ do
>       logInfo "Hello World"

VERSION 3

> data SimpleApp =
>   SimpleApp {
>     appLogFunc     :: !LogFunc
>   , appProcessContext :: !ProcessContext
>   }
>
> instance HasLogFunc SimpleApp where
>   logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
>
> instance HasProcessContext SimpleApp where
>   processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })
>

> mainProc :: IO ()
> mainProc = do
>   appProcessContext <- mkDefaultProcessContext
>   logOptions <- logOptionsHandle stdout True
>   withLogFunc logOptions $ \appLogFunc ->
>     runRIO (SimpleApp {appLogFunc, appProcessContext}) $ do
>       proc "ls" ["-lh"] (runProcess_)
>
>

Pick on of the main functions here

> main :: IO ()
> main = mainProc

A few questions:

* What happens if you try to use @putStrLn@?

* Do functions work inside and outside the @runRIO@ invocation, why?

* What happens if you try to use functions that fail at runtime? e.g. head?
