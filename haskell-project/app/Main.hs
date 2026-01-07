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
    ["--host"] -> mainServer
    [ip, port] -> mainClient ip (read port)
    _ -> putStrLn "Usage: program --host  OR  program <ip> <port>"