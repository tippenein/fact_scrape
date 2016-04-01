module ScraperSpec (spec) where

import Politifact.Scraper

import Test.Hspec
import qualified Data.Text as T
import Data.Time.Calendar (fromGregorian)

spec :: Spec
spec = do
  describe "date parsing" $ do
    it "returns the correctly parsed date" $ do
      (pullDate (T.pack "/whatever/blah/2014/mar/2")) `shouldBe` (fromGregorian 2014 3 2)
