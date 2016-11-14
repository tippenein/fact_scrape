{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

-- | Implementation of the truth API.
module Truth.Server.Handlers
  ( server
  ) where

import Protolude

import Control.Monad.Except (ExceptT(..))
import Control.Monad.Log (Severity, logInfo)
import Servant (ServantErr, Server, (:<|>)(..), (:~>)(..), enter, err404)
import Text.PrettyPrint.Leijen.Text (Doc, Pretty, text)
import Database.Persist
import Database.Persist.Sqlite
import Data.List (groupBy, sort)

import Truth.API
import Truth.Server.Database as Database
import qualified Truth.Server.Logging as Log


handlers =
       listPersons
  :<|> findPerson
  :<|> listStatements
  :<|> listTruthiness

-- | truth API implementation.
server :: Severity -> Server API
server logLevel = enter (toHandler logLevel) handlers

-- | Our custom handler type.
type Handler msg = ExceptT ServantErr (Log.LogM msg IO)

-- | Translate our custom monad into a Servant handler.
--
-- See http://haskell-servant.readthedocs.io/en/stable/tutorial/Server.html#using-another-monad-for-your-handlers
-- for the details.
toHandler
  :: Pretty msg
  => Severity -> (Handler msg :~> ExceptT ServantErr IO)
toHandler logLevel = Nat toHandler'
  where
    toHandler'
      :: Pretty msg
      => Handler msg a -> ExceptT ServantErr IO a
    toHandler' = ExceptT . Log.withLogging logLevel . runExceptT

findPerson :: Int64 -> Handler Doc (Entity Person)
findPerson person_id = do
  ms <- liftIO $ Database.findPersonMaybe person_id
  case ms of
    Nothing -> throwIO err404
    Just m -> do
      pure $ Entity (toSqlKey person_id) m

listTruthiness :: Int64 -> Handler Doc [PersonTruthiness]
listTruthiness person_id = do
  ms <- liftIO $ Database.selectStatements person_id
  pure $ makePT ms

groupByTruth = groupBy (\a b -> personStatementTruthValue a == personStatementTruthValue b)

--XXX smellls
makePT :: [PersonStatement] -> [PersonTruthiness]
makePT statements = catMaybes $ fmap truthTotals grouped
  where
    grouped = (groupByTruth . sort) statements
    truthTotals s = case head s of
      Just t -> Just $ PersonTruthiness
                  { tVal = personStatementTruthValue t
                  , total = length s }
      Nothing -> Nothing

listPersons :: Maybe Text -> Handler Doc [Entity Person]
listPersons mquery = do
  logInfo (text "fetching all Persons")
  liftIO $ Database.selectPersons mquery

listStatements :: Maybe Text -> Handler Doc [PersonStatement]
listStatements name = do
  statements <- liftIO $ Database.selectStatementsByName name
  case statements of
    Nothing -> pure []
    Just r -> pure r
