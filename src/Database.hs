{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Database where

-- import Data.Time.Clock (UTCTime (..))
import Data.Text (Text, unpack)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.List (groupBy)
-- import Database.Esqueleto
import GHC.Generics
import Data.Time.Calendar (Day)
-- import Control.Monad (mapM_)

import Politifact.Scraper

runDb = runSqlite "statements.db"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person json
    name Text
    deriving Eq Show Generic

PersonStatement json
    person Person
    truthValue Text
    statedOn Day
    statementLink Text
    UniqueStatement person statementLink
    deriving Eq Show Generic
|]

groupByTruth = groupBy (\a b -> personStatementTruthValue a == personStatementTruthValue b)
instance Ord PersonStatement where
  (PersonStatement _ t1 _ _ ) `compare` (PersonStatement _ t2 _ _) = t1 `compare` t2

persistValue (Entity _ v) = v

migrateDb:: IO ()
migrateDb = runDb $ runMigration migrateAll

findPerson :: Text -> IO (Maybe (Entity Person))
findPerson str = do
    people <- runDb $ selectList [PersonName ==. str] []
    case length people of
      0 -> return Nothing
      _ -> return $ Just (head people)

findOrCreatePersonByName :: Text -> IO Person
findOrCreatePersonByName name = do
  p <- findPerson name
  case p of
    Nothing -> do
      _ <- runDb $ insert $ Person name
      putStrLn $ "inserted " ++ unpack name
      return $ Person name
    Just person -> return $ persistValue person

insertStatement :: PoliticalStatement -> IO ()
insertStatement s = do
  person <- findOrCreatePersonByName (name s)
  runDb $ insert_ $ PersonStatement person (truth s) (statedOn s) (statementLink s)

insertStatements :: [PoliticalStatement] -> IO ()
insertStatements statements = do
  mapM_ insertStatement statements

selectPersons :: IO [Person]
selectPersons = do
  dbPersons <- runDb $ selectList [] []
  return $ map persistValue dbPersons


-- TODO fix this asap
selectStatements :: Maybe Text -> IO [PersonStatement]
selectStatements name = do
  s <- runDb $ selectList [] []
  let statements = map persistValue s
  case name of
    Nothing -> return statements
    Just n  -> return $ filterName n statements

filterName query = filter (\a -> query == (personName . personStatementPerson) a)

-- insertStatementsConsumer = do
--   runEffect $ for Scraper.getAll $ \statement -> do
--      lift $ print statement
