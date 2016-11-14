{-# LANGUAGE QuasiQuotes #-}
module Main
  ( main
  ) where

import Protolude
import qualified Prelude

import Truth.Scraper.Politifact
import Test.Tasty (defaultMain, TestTree, testGroup)
import qualified Data.Text as T
import Test.Tasty.Hspec (Spec, it, testSpec)
import Test.Hspec
import Data.Time.Calendar (fromGregorian)
import Text.XML.HXT.Core


main :: IO ()
main = defaultMain =<< tests

tests :: IO TestTree
tests = do
  specs <- testSpec "quickcheck tests" scraperSpec
  pure $ testGroup "Truth.Scraper" [specs]

scraperSpec :: Spec
scraperSpec = do
  describe "date parsing" $ do
    it "returns the correctly parsed date" $ do
      (pullDate (T.pack "/whatever/blah/2014/mar/2")) `shouldBe` (fromGregorian 2014 3 2)

  describe "#statementsFor" $ do
    it "constructs statements from a document" $ do
      statements <- statementsFor =<< testDoc "latest-facts"

      (length statements) `shouldBe` 20

      ((name . Prelude.head) statements) `shouldBe` "Donald Trump"

    it "constructs statements with unicode characters in it" $ do
      statements <- statementsFor =<< testDoc "unicode-error"

      ((name . Prelude.head) statements) `shouldBe` "Donald ćrump"

testDoc bodyName = do
  html <- Prelude.readFile $ "tests/fixtures/" ++ bodyName ++ ".html"
  let doc = readString [withParseHTML yes, withWarnings no] html
  pure doc

