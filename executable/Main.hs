module Main where

import qualified Control.Exception       as Exception
import           Control.Monad           (liftM)
import Database
import Server
import Politifact.Scraper as Scraper

seedDb = do
  migrateDb
  insertStatements =<< Scraper.getAll

main :: IO ()
main = do
  seedDb
  let port = 8081 :: Int
  putStrLn ("Starting on port " ++ show port ++ "...")
  Exception.catch
    (runServer port)
    (\ Exception.UserInterrupt -> putStrLn "\nStopping...")
