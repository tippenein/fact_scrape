{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module WebSpec (spec) where

import Api
import Server (app)

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

spec :: Spec
spec = do
  apiSpec

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
