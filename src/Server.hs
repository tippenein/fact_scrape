{-# LANGUAGE OverloadedStrings #-}
module Server (runServer, app) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either
import Data.Int (Int64)
import Data.List (groupBy, sort)
import Data.Text (Text)
import Database.Persist
import Database.Persist.Sqlite
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger (logStdout)
import Servant

import Api
import Database

server :: Server TruthApi
server =
       listPersons
  :<|> findPerson
  :<|> listStatements
  :<|> listTruthiness

type Handler a = EitherT ServantErr IO a

findPerson :: Int64 -> Handler (Entity Person)
findPerson person_id = do
  ms <- liftIO $ Database.findPersonMaybe person_id
  case ms of
    Nothing -> left err404
    Just m -> right (Entity (toSqlKey person_id) m)

listTruthiness :: Int64 -> Handler [PersonTruthiness]
listTruthiness person_id = do
  ms <- liftIO $ Database.selectStatements person_id
  return $ makePT ms

groupByTruth = groupBy (\a b -> personStatementTruthValue a == personStatementTruthValue b)

makePT :: [PersonStatement] -> [PersonTruthiness]
makePT statements = map truthTotals grouped
  where
    grouped = (groupByTruth . sort) statements
    truthTotals s = PersonTruthiness
      { tVal = personStatementTruthValue $ head s
        , total = length s }

listPersons :: Handler [Entity Person]
listPersons =
  liftIO Database.selectPersons

listStatements :: Maybe Text -> Handler [PersonStatement]
listStatements name = do
  statements <- liftIO $ Database.selectStatementsByName name
  case statements of
    Nothing -> return []
    Just r -> right r

middlewares = simpleCors . logStdout

app :: Application
app = middlewares $ serve Api.truthApi server

runServer :: Port -> IO ()
runServer port = run port app
