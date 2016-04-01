{-# LANGUAGE OverloadedStrings #-}

module Politifact.Scraper where

import qualified Data.Text as T
import Pipes
import Control.Monad (mapM)
import Control.Concurrent (threadDelay)
import Text.HandsomeSoup
import Text.XML.HXT.Core

baseUrl = "http://www.politifact.com"

statementUrl = "/truth-o-meter/statements/?page="

data Statement
  = Statement
  { truth         :: T.Text
  , name          :: T.Text
  , statementLink :: T.Text
  } deriving (Show)

buildStatement :: [(String,String,String)] -> [Statement]
buildStatement = fmap (\(a,b,c) -> Statement (T.pack a) (T.pack b) (T.pack c))

statementsForPage :: Int -> IO [Statement]
statementsForPage i = do
  let doc = fromUrl (baseUrl ++ statementUrl ++ show i)
  truths <- runX $ doc >>> css "div.meter img" ! "alt"
  names <-  runX $ doc >>> css "div.mugshot img" ! "alt"
  statements <- runX $ doc >>> css "p.statement__text a" ! "href"
  return $ buildStatement (zip3 truths names statements)

-- getAll :: Proxy p => Producer p [Statement] IO ()
getAll = do
  s <- mapM (\i -> threadDelay 1000000 >> statementsForPage i) [1] -- 196
  return $ concat s

-- getAllProducer= do
--   s <- mapM (\i -> threadDelay 1000000 >> statementsForPage i) [1..196]
--   yield $ concat s


