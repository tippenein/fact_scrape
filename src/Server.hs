{-# LANGUAGE OverloadedStrings #-}
module Server (runServer, app) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either
import Data.Text (Text)
import Data.List (sort)
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
  :<|> listStatements
  :<|> listTruthiness

type Handler a = EitherT ServantErr IO a


listTruthiness :: Maybe Text -> Handler [(Person, PersonTruthiness)]
listTruthiness person_name = do
  ms <- liftIO $ Database.selectStatements person_name
  return $ fmap makePT $ (groupByTruth . sort) ms

makePT :: [PersonStatement] -> (Person, PersonTruthiness)
makePT s = (personStatementPerson $ head s, toPT s)

toPT statements = PersonTruthiness {
  tVal = (personStatementTruthValue $ head statements),
  total = length statements}

listPersons :: Handler [Person]
listPersons =
  liftIO Database.selectPersons

listStatements :: Maybe Text -> Handler [PersonStatement]
listStatements name = do
  ms <- liftIO $ Database.selectStatements name
  return $ ms

middlewares = simpleCors . logStdout

app :: Application
app = middlewares $ serve Api.truthApi server

runServer :: Port -> IO ()
runServer port = run port app
