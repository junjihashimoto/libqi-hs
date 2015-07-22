{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

import Qi
import System.Environment
import qualified Data.ByteString as BS
import Control.Concurrent

main = do
  [url] <- getArgs
  session <- createSession url
  say session "hello"
  speachReco session ["yes","no"] $ \str -> do
    say session str
  threadDelay (1000*1000*30)
  destroySession session
  return ()
