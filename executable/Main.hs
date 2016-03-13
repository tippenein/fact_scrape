module Main where

import qualified Politifact.Scraper as Scraper

main :: IO ()
main = do
  d <- Scraper.statementsForPage 1
  mapM_ print d
