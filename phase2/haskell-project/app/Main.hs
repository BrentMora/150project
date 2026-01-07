{-# LANGUAGE OverloadedStrings #-}

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
    [] -> mainClient "127.0.0.1" 15000
    _ -> putStrLn "Usage: program        (start client)\n       program --host (start server)"