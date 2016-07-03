module MigratePersonStatementsPersonId where

import Data.Foldable
import Database
import Database.Persist

selectOldStatements :: IO [PersonStatementOld]
selectOldStatements = do
  olds <- runDb $ selectList [] []
  return $ map entityVal olds

moveToNewTable :: IO ()
moveToNewTable = do
  olds <- selectOldStatements
  traverse_ insertNewStatementFrom olds

insertNewStatementFrom :: PersonStatementOld -> IO ()
insertNewStatementFrom old = do
  pid <- findOrCreatePersonByName (personName (personStatementOldPerson old))
  _ <- runDb $ insert $
      PersonStatement pid (personStatementOldTruthValue old) (personStatementOldStatedOn old) (personStatementOldStatementLink old)
  return ()
