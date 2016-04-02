{-# LANGUAGE OverloadedStrings #-}
module ScraperSpec (spec) where

import Politifact.Scraper

import Test.Hspec
import qualified Data.Text as T
import Data.Time.Calendar (fromGregorian)
import Text.XML.HXT.Core

testDoc bodyName = do
  html <- readFile $ "test/fixtures/" ++ bodyName ++ ".html"
  let doc = readString [withParseHTML yes, withWarnings no] html
  return doc


spec :: Spec
spec = do
  describe "date parsing" $ do
    it "returns the correctly parsed date" $ do
      (pullDate (T.pack "/whatever/blah/2014/mar/2")) `shouldBe` (fromGregorian 2014 3 2)

  describe "#statementsFor" $ do
    it "constructs statements from a document" $ do
      statements <- statementsFor =<< testDoc "latest-facts"

      (length statements) `shouldBe` 20

      ((name . head) statements) `shouldBe` "Donald Trump"

    it "constructs statements with unicode characters in it" $ do
      statements <- statementsFor =<< testDoc "unicode-error"

      ((name . head) statements) `shouldBe` "Donald ćrump"
