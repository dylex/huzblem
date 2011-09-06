module Database 
  ( Database
  , databaseOpen
  , databaseClose
  , browseAdd
  , browseFind
  , browseFavorites
  , browseSetTitle
  , blockTest
  , blockLists
  , blockSet
  ) where

import Control.Concurrent.MVar
import Control.Monad
import Data.Array
import Data.Maybe
import Data.Time.LocalTime (LocalTime)

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
  | BrowseFavorites
  | BrowseSetTitle
  | BlockTest
  | BlockList
  | BlockSet
  | BlockAdd
  deriving (Show, Eq, Ord, Bounded, Enum, Ix)

queries :: [String]
queries = 
  [ "SELECT browse_add(?, ?)"
  , "SELECT uri FROM browse WHERE text(uri) LIKE '%' || ? || '%' ORDER BY last DESC LIMIT 1"
  , "SELECT text(uri), title, last FROM (SELECT uri, title, last, \
    \   sum(visits) OVER d as v, row_number() OVER d AS i \
    \ FROM browse WINDOW d AS (\
    \   PARTITION BY (uri).domain ORDER BY last DESC ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING \
    \ )) b WHERE i = 1 ORDER BY v DESC"
  , "UPDATE browse SET title = ? WHERE uri = ?::uri"
  , "SELECT trust FROM block WHERE domain = ANY (domainname_parents(?)) ORDER BY length(domain) DESC LIMIT 1"
  , "SELECT domain FROM block WHERE trust = ? ORDER BY domain"
  , "UPDATE block SET trust = ? WHERE domain = ?"
  , "INSERT INTO block (trust, domain) VALUES (?, ?)"
  ]

withQuery' :: QueryType -> (Statement -> IO a) -> Database -> IO a
withQuery' qt f d = withMVar (databaseQueries d ! qt) f

withQuery :: QueryType -> (Statement -> IO a) -> Database -> IO a
withQuery qt f = withQuery' qt $ \q -> do
  r <- f q
  finish q
  return r

execute_ :: Statement -> [SqlValue] -> IO ()
execute_ s = void . execute s

browseAdd :: String -> Maybe String -> Database -> IO ()
browseAdd u t = withQuery BrowseAdd (`execute_` [SqlString u, toSql t])

browseFind :: String -> Database -> IO (Maybe String)
browseFind u = withQuery BrowseFind $ \q -> do
  execute_ q [SqlString u]
  fmap (fromSql . head) =.< fetchRow q

browseFavorites :: Database -> IO [(String, Maybe String, LocalTime)]
browseFavorites = withQuery' BrowseFavorites $ \q -> do
  execute_ q []
  map (\[u,t,l] -> (fromSql u, fromSql t, fromSql l)) =.< fetchAllRows q

browseSetTitle :: String -> String -> Database -> IO ()
browseSetTitle u t = withQuery BrowseSetTitle (`execute_` [SqlString u, SqlString t])

blockTest :: String -> Database -> IO (Maybe Bool)
blockTest h = withQuery BlockTest $ \q -> do
  execute_ q [SqlString h]
  (fromSql . head =<<) =.< fetchRow q

blockLists :: Database -> IO ([String], [String])
blockLists = withQuery BlockList $ \q ->
  let bl t = do
        execute_ q [SqlBool t]
        mapMaybe (fromSql . head) =.< fetchAllRows' q
  in liftM2 (,) (bl False) (bl True)

blockSet :: String -> Maybe Bool -> Database -> IO ()
blockSet h t d = do
  r <- withQuery BlockSet e d
  when (r == 0) $ withQuery BlockAdd (void . e) d
  where e q = execute q [toSql t, SqlString h]

databaseOpen :: IO Database
databaseOpen = do
  c <- connectPostgreSQL' "dbname=uzbl"
  q <- mapM (newMVar <=< prepare c) queries
  return $ Database c $ listArray (minBound, maxBound) q

databaseClose :: Database -> IO ()
databaseClose = disconnect . databaseConnection
