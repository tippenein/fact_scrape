module Main where

import qualified Control.Exception as Exception

import Database
import Politifact.Scraper as Scraper
import Server
import System.Environment

main :: IO ()
main = do
  [args] <- getArgs
  case args of
    "serve" -> serveIt
    "scrape" -> insertStatements =<< Scraper.getPageRange 1 1
    "history" -> insertStatements =<< Scraper.getHistoric
    "migrate" -> migrateDb
    _ -> putStrLn "try 'serve', 'scrape' or 'history'"

startWithPort port = do
  putStrLn ("Starting on port " ++ show port ++ "...")
  Exception.catch
    (runServer port)
    (\ Exception.UserInterrupt -> putStrLn "\nStopping...")


serveIt = do
  migrateDb
  port <- getEnv "PORT"
  case port of
    "" -> startWithPort 8003
    _ -> startWithPort (read port)
