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

import Data.Csv
import Data.Text (Text)
import Data.Time.Clock (UTCTime (..))
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Generics

runDb = runSqlite "statements.db"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name Text
    deriving Eq Show Generic

PersonStatement
    person Person
    content Text
    statedOn UTCTime
    deriving Eq Show Generic

Truthiness
    person Person
    truthValue Text
    deriving Eq Show Generic
|]
