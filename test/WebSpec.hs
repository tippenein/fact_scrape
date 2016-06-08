{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module WebSpec (spec) where

import Server (app)

import Test.Hspec
import Test.Hspec.Wai hiding (pending)
import Test.Hspec.Wai.JSON

spec :: Spec
spec =
  apiSpec

apiSpec :: Spec
apiSpec = with (return app) $ do
  describe "GET /persons" $
    it "responds with 200" $
      get "/persons" `shouldRespondWith` 200

  describe "GET /persons/:id" $ do
    it "responds with 200" $
      get "/persons/1" `shouldRespondWith` 200

    it "responds with 404" $
      get "/persons/9001" `shouldRespondWith` 404

  describe "GET /statements" $
    it "responds with 200" $
      get "/statements" `shouldRespondWith` 200

  describe "GET /statements with non-existentent name" $
    it "responds with empty" $
      get "/statements?person_name=Nope" `shouldRespondWith` [json|[]|]
