{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
module Main where

import Client (mainClient)
import Server (mainServer)
import System.Environment (getArgs)

-- Main entry point of the application
-- Parses command line arguments to determine whether to run as server or client
main :: IO ()
main = do
  -- Get command line arguments
  args <- getArgs
  -- Print arguments for debugging
  print args
  -- Pattern match on arguments to determine mode
  case args of
    ["--host"] -> mainServer                          -- If "--host" flag, run as server
    [ip, port] -> mainClient ip (read port)           -- If ip and port provided, run as client
    _ -> putStrLn "Usage: program --host  OR  program <ip> <port>"  -- Invalid arguments