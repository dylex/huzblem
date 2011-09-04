module Database 
  ( Database
  , databaseOpen
  , databaseClose
  , browseAdd
  , browseFind
  , blockTest
  , blockLists
  , blockSet
  ) where

import Control.Concurrent.MVar
import Control.Monad
import Data.Array
import Data.Maybe

import Database.HDBC
import Database.HDBC.PostgreSQL

import Util

type Query = MVar Statement

data Database = Database
  { databaseConnection :: Connection
  , databaseQueries :: Array QueryType Query
  }

data QueryType
  = BrowseAdd
  | BrowseFind
  | BlockTest
  | BlockList
  | BlockSet
  | BlockAdd
  deriving (Eq, Ord, Bounded, Enum, Ix)

queries :: [String]
queries = 
  [ "SELECT browse_add(?)"
  , "SELECT uri FROM browse WHERE uri LIKE '%' || ? || '%' ORDER BY last DESC LIMIT 1"
  , "SELECT trust FROM block WHERE host = ANY (domainname_parents(?))"
  , "SELECT host FROM block WHERE trust = ? ORDER BY host"
  , "UPDATE block SET trust = ? WHERE host = ?"
  , "INSERT INTO block (trust, host) VALUES (?, ?)"
  ]

withQuery :: Database -> QueryType -> (Statement -> IO a) -> IO a
withQuery d qt f = withMVar (databaseQueries d ! qt) $ \q -> do
  r <- f q
  finish q
  return r

execute_ :: Statement -> [SqlValue] -> IO ()
execute_ s = void . execute s

browseAdd :: String -> Database -> IO ()
browseAdd u d = withQuery d BrowseAdd $ (`execute_` [SqlString u])

browseFind :: String -> Database -> IO (Maybe String)
browseFind u d = withQuery d BrowseFind $ \q -> do
  execute_ q [SqlString u]
  fmap (fromSql . head) =.< fetchRow q

blockTest :: String -> Database -> IO (Maybe Bool)
blockTest h d = withQuery d BlockTest $ \q -> do
  execute_ q [SqlString h]
  fmap (fromSql . head) =.< fetchRow q

blockLists :: Database -> IO ([String], [String])
blockLists d = withQuery d BlockList $ \q ->
  let bl t = do
        execute_ q [SqlBool t]
        mapMaybe (fromSql . head) =.< fetchAllRows' q
  in liftM2 (,) (bl False) (bl True)

blockSet :: String -> Maybe Bool -> Database -> IO ()
blockSet h t d = do
  r <- withQuery d BlockSet e
  when (r == 0) $ withQuery d BlockAdd $ void . e
  where e q = execute q [toSql t, SqlString h]

databaseOpen :: IO Database
databaseOpen = do
  c <- connectPostgreSQL' "dbname=uzbl"
  q <- mapM (newMVar <=< prepare c) queries
  return $ Database c $ listArray (minBound, maxBound) q

databaseClose :: Database -> IO ()
databaseClose = disconnect . databaseConnection
