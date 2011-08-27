module Database 
  ( Database
  , databaseOpen
  , databaseClose
  , browseAdd
  ) where

import Control.Monad
import Data.Array

import Database.HDBC
import Database.HDBC.PostgreSQL

data Query 
  = BrowseAdd
  deriving (Eq, Ord, Bounded, Enum, Ix)

data Database = Database
  { databaseConnection :: Connection
  , databaseQueries :: Array Query Statement
  }

queries :: [String]
queries = 
  [ "SELECT browse_add(?)"
  ]

query :: Database -> Query -> Statement
query = (!) . databaseQueries

browseAdd :: String -> Database -> IO ()
browseAdd u d = void $ execute (query d BrowseAdd) [toSql u]

databaseOpen :: IO Database
databaseOpen = do
  c <- connectPostgreSQL' ""
  runRaw c "SET search_path = uzbl"
  q <- mapM (prepare c) queries
  return $ Database c $ listArray (minBound, maxBound) q

databaseClose :: Database -> IO ()
databaseClose = disconnect . databaseConnection
