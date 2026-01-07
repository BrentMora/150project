{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import Client (mainClient)
import Server (mainServer)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  print args
  case args of
    ["--host", portStr] -> mainServer (read portStr)
    [ip, portStr] -> mainClient ip (read portStr)
    _ -> pure ()