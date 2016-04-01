module Main where

import Politifact.Scraper as Scraper
import Database

seedDb = do
  migrateDb
  insertStatements =<< Scraper.getAll

main :: IO ()
main = seedDb
