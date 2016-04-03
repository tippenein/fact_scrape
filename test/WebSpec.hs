{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module WebSpec (spec) where

import Server (app)

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Data.Time.Calendar (fromGregorian)

import Data.List
import Database

spec :: Spec
spec = do
  apiSpec
  utilSpec


utilSpec :: Spec
utilSpec = do
  describe "orderable PersonStatement" $ do
    it "should group by truth values" $ do
      let s1 = PersonStatement (Person "a") "truth" (fromGregorian 2012 1 1) "a"
          s2 = PersonStatement (Person "b") "truth" (fromGregorian 2012 1 1) "b"
          s3 = PersonStatement (Person "c") "lying" (fromGregorian 2012 1 1) "c"
        in
        -- ((groupByTruth . sort) [s1,s2,s3]) `shouldBe` [[s1,s2],[s3]]

          (length  ((groupByTruth . sort) [s1,s2,s3])) `shouldBe` 2

apiSpec :: Spec
apiSpec = with (return app) $ do
  describe "GET /persons" $ do
    it "responds with 200" $ do
      get "/persons" `shouldRespondWith` 200

  describe "GET /statements" $ do
    it "responds with 200" $ do
      get "/statements" `shouldRespondWith` 200

  describe "GET /statements with non-existentent name" $ do
    it "responds with empty" $ do
      get "/statements?person_name=Nope" `shouldRespondWith` [json|[]|]
