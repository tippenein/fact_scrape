{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module WebSpec (spec) where

import Api
import Database
import Server (server)

import qualified Control.Exception as Exception
import Data.Aeson (ToJSON, encode)
import Data.ByteString (ByteString)
import Database.Persist (deleteWhere, insert, (>=.))
import Database.Persist.Sql (toSqlKey)
import Database.Persist.Sqlite (runMigration, runSqlConn, withSqliteConn)
import Network.HTTP.Types.Method (methodPost)
import Network.Wai.Test (SResponse)
import Servant.Server (serve)
import System.Directory
import qualified System.Environment as Env
import Test.Hspec
import Test.Hspec.Wai hiding (pending)
import Test.Hspec.Wai.JSON

spec :: Spec
spec =
  apiSpec

apiSpec :: Spec
apiSpec = with app $ do
  describe "GET /persons" $ do
    it "responds with 200" $
      get "/persons" `shouldRespondWith` 200

    it "responds with correct payload" $ do
      get "/persons/1" `shouldRespondWith` [json|{name: "derp", id: 1}|]

  describe "GET /persons?q=not" $ do
    it "responds with only the matching person" $
      get "/persons?q=not" `shouldRespondWith` [json|[{name: "not", id: 2}]|]

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

postJson :: (ToJSON a) => ByteString -> a -> WaiSession SResponse
postJson path =
  request methodPost path [("Content-Type", "application/json")] . encode

withTestDb = Exception.bracket setTest unsetTest
  where
    setTest = Env.setEnv "SERVANT_ENV" "test"
    unsetTest = do
      _ <- const $ removeFile "statement.test.db"
      const $ Env.unsetEnv "SERVANT_ENV"

app = withTestDb $ do
  const $ runDb $ do
    runMigration migrateAll
    deleteWhere [PersonId >=. toSqlKey 0]
    _ <- insert $ Person "derp"
    _ <- insert $ Person "not"
    return . serve truthApi $ server
