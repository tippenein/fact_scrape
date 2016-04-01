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

import Data.Time.Clock (UTCTime (..))
import Data.Text (Text, unpack)
import Database.Persist
import Database.Persist.Sqlite hiding (Statement)
import Database.Persist.TH
import GHC.Generics
import Control.Monad (mapM_)

import Politifact.Scraper

runDb = runSqlite "statements.db"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name Text
    deriving Eq Show Generic

PersonStatement
    person Person
    truthValue Text
    statementLink Text
    UniqueStatement person statementLink
    deriving Eq Show Generic
|]

-- PersonStatement
--     person Person
--     content Text
--     statedOn UTCTime
--     truthValue Text
--     deriving Eq Show Generic
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

insertStatement s = do
  person <- findOrCreatePersonByName (name s)
  runDb $ insert_ $ PersonStatement person (truth s) (statementLink s)

insertStatements :: [Statement] -> IO ()
insertStatements statements = do
  mapM_ (\s -> insertStatement s) statements

-- insertStatementsConsumer = do
--   runEffect $ for Scraper.getAll $ \statement -> do
--      lift $ print statement
