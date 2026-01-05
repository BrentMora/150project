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
    [] -> mainClient "127.0.0.1" 15000                -- No args: run as client connecting to localhost:15000
    _ -> putStrLn "Usage: program --host  OR  program (no args for client)"  -- Invalid arguments