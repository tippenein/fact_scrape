{-# LANGUAGE OverloadedStrings #-}

module Scraper where

import Text.HandsomeSoup
import Text.XML.HXT.Core

url :: String
url = "http://www.politifact.com/truth-o-meter/statements/?page="

data Statement
  = Statement
  { truth         :: String
  , name          :: String
  , statementLink :: String
  } deriving (Show)

buildStatement :: [(String,String,String)] -> [Statement]
buildStatement = fmap (\(a,b,c) -> Statement a b c)

statementsForPage :: Int -> IO [Statement]
statementsForPage i = do
  let doc = fromUrl (url ++ show i)
  truths <- runX $ doc >>> css "div.meter img" ! "alt"
  names <- runX $ doc >>> css "div.mugshot img" ! "alt"
  statements <- runX $ doc >>> css "p.statement__text a" ! "href"
  return $ buildStatement (zip3 truths names statements)

