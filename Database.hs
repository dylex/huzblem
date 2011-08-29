module Database 
  ( Database
  , databaseOpen
  , databaseClose
  , browseAdd
  , browseFind
  ) where

import Control.Monad
import Data.Array

import Database.HDBC
import Database.HDBC.PostgreSQL

import Util

data Query 
  = BrowseAdd
  | BrowseFind
  deriving (Eq, Ord, Bounded, Enum, Ix)

data Database = Database
  { databaseConnection :: Connection
  , databaseQueries :: Array Query Statement
  }

queries :: [String]
queries = 
  [ "SELECT browse_add(?)"
  , "SELECT uri FROM browse WHERE uri LIKE '%' || ? || '%' ORDER BY last DESC LIMIT 1"
  ]

query :: Database -> Query -> Statement
query = (!) . databaseQueries

browseAdd :: Database -> String -> IO ()
browseAdd d u = void $ execute (query d BrowseAdd) [SqlString u]

browseFind :: Database -> String -> IO (Maybe String)
browseFind d u = do
  void $ execute q [SqlString u]
  fmap (fromSql . head) =.< fetchRow q
  where q = query d BrowseFind

databaseOpen :: IO Database
databaseOpen = do
  c <- connectPostgreSQL' ""
  runRaw c "SET search_path = uzbl"
  q <- mapM (prepare c) queries
  return $ Database c $ listArray (minBound, maxBound) q

databaseClose :: Database -> IO ()
databaseClose = disconnect . databaseConnection
