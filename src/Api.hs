{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeOperators  #-}

module Api where

import Data.Aeson
import Data.Int (Int64)
import Data.Proxy
import Data.Text (Text)
import Database
import Database.Persist
import GHC.Generics
import Servant.API
import Servant.JS

data PersonTruthiness = PersonTruthiness
  { tVal  :: Text
  , total :: Int
  } deriving (Show, ToJSON, Generic, Eq)

truthApi :: Proxy TruthApi
truthApi = Proxy

type TruthApi =
       ListPersons
  :<|> FindPerson
  :<|> ListStatements
  :<|> ListTruthiness

type ListPersons = "persons" :> Get '[JSON] [Entity Person]

type FindPerson =
     "persons"
  :> Capture "person_id" Int64
  :> Get '[JSON] (Entity Person)

type ListStatements =
     "statements"
  :> QueryParam "person_name" Text
  :> Get '[JSON] [PersonStatement]

type ListTruthiness =
     "persons"
  :> Capture "person_id" Int64
  :> "truthiness"
  :> Get '[JSON] [PersonTruthiness]

generateJavaScript :: IO ()
generateJavaScript = writeJSForAPI (Proxy :: Proxy TruthApi) vanillaJS "../site/assets/api.js"
