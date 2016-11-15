module Main
  ( main
  ) where

import Protolude

import Truth.Server.Database (insertStatements)
import System.Environment

main :: IO ()
main = do
  [args] <- getArgs
  case args of
    "scrape" -> insertStatements =<< Scraper.getPageRange 1 1
    "history" -> insertStatements =<< Scraper.getHistoric
    _ -> error "try scrape or history"
