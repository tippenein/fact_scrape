{-# LANGUAGE OverloadedStrings #-}

module Politifact.Scraper where

import Control.Concurrent (threadDelay)
import qualified Data.Text as T
import Data.Time.Calendar (Day, fromGregorian)
import Text.HandsomeSoup
import Text.XML.HXT.Core

baseUrl = "http://www.politifact.com"

statementUrl = "/truth-o-meter/statements/?page="

data PoliticalStatement
  = PoliticalStatement
  { truth         :: T.Text
  , name          :: T.Text
  , statedOn      :: Day
  , statementLink :: T.Text
  } deriving (Show)

buildStatement :: [(String,String,String)] -> [PoliticalStatement]
buildStatement = fmap (\(a,b,c) ->
  PoliticalStatement {
    truth = T.pack a,
    name = T.strip $ T.pack b,
    statementLink = T.pack c,
    statedOn = pullDate (T.pack c)}
  )

pullDate :: T.Text -> Day
pullDate c = fromGregorian y m d
  where
    y = read (url !! 3) :: Integer
    m = monthFromString $ url !! 4
    d = read (url !! 5) :: Int
    url = map T.unpack $ T.splitOn "/" c

monthFromString s =
  case s of
    "jan" -> 1
    "feb" -> 2
    "mar" -> 3
    "apr" -> 4
    "may" -> 5
    "jun" -> 6
    "jul" -> 7
    "aug" -> 8
    "sep" -> 9
    "oct" -> 10
    "nov" -> 11
    "dec" -> 12
    _     -> error "oh noes"

statementsFor :: HtmlDoc -> IO [PoliticalStatement]
statementsFor doc = do
  truths <- runX $ doc >>> css "div.meter img" ! "alt"
  names <-  runX $ doc >>> css "div.mugshot img" ! "alt"
  statements <- runX $ doc >>> css "p.statement__text a" ! "href"
  return $ buildStatement (zip3 truths names statements)

type HtmlDoc = IOSArrow XmlTree XmlTree

docFromPage :: Integer -> HtmlDoc
docFromPage i = fromUrl (baseUrl ++ statementUrl ++ show i)

getPageRange start finish = do
  s <- mapM (\i -> threadDelay 1000000 >> statementsFor (docFromPage i)) [start..finish]
  return $ concat s

getPage n =
  return $ statementsFor $ docFromPage n

getHistoric = getPageRange 1 196

-- data StatementBody = StatementBody { synopsis :: T.Text }

statementBodyFor :: T.Text -> IO T.Text
statementBodyFor link = do
  doc <- pure $ fromUrl (baseUrl ++ T.unpack link)
  body <- runX $ doc >>> css "div.statement__text" >>> isText >>> getText
  return $ T.pack (head body)
