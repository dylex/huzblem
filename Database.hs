module Database 
  ( Database
  , databaseOpen
  , databaseClose
  , browseAdd
  , browseFind
  , browseFavorites
  , browseSetTitle
  , markAdd
  , markList
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
  | BrowseFavorites
  | BrowseSetTitle
  | MarkAdd
  | MarkList
  | BrowseFind
  deriving (Show, Eq, Ord, Bounded, Enum, Ix)

queries :: [String]
queries = 
  [ "SELECT browse_add(?, ?)"
  , "SELECT text(uri), title, last FROM (SELECT uri, title, last, \
    \   row_number() OVER d AS i \
    \ FROM browse WINDOW d AS (\
    \   PARTITION BY (uri).domain ORDER BY last DESC ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING \
    \ )) b WHERE i = 1 ORDER BY last DESC"
  , "UPDATE browse SET title = ? WHERE uri = ?::uri"
  , "SELECT mark_add(?, ?)"
  , "SELECT COALESCE(browse.uri, mark.uri), browse.title, browse.last \
    \ FROM mark LEFT JOIN browse ON (mark.browse = browse.id) \
    \ ORDER BY last DESC"
  , "SELECT COALESCE(browse.uri, mark.uri) \
    \ FROM mark FULL JOIN browse ON (mark.browse = browse.id) \
    \ WHERE text(coalesce(mark.uri, browse.uri)) LIKE '%' || ? || '%' \
    \ ORDER BY mark.id IS NULL, browse.last DESC LIMIT 1"
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

browseFavorites :: Database -> IO [(String, Maybe String, Maybe LocalTime)]
browseFavorites = withQuery' BrowseFavorites $ \q -> do
  execute_ q []
  map (\[u,t,l] -> (fromSql u, fromSql t, fromSql l)) =.< fetchAllRows q

browseSetTitle :: String -> String -> Database -> IO ()
browseSetTitle u t = withQuery BrowseSetTitle (`execute_` [SqlString u, SqlString t])

markAdd :: String -> Bool -> Database -> IO ()
markAdd u f = withQuery MarkAdd (`execute_` [SqlString u, SqlBool f])

markList :: Database -> IO [(String, Maybe String, Maybe LocalTime)]
markList = withQuery' MarkList $ \q -> do
  execute_ q []
  map (\[u,t,l] -> (fromSql u, fromSql t, fromSql l)) =.< fetchAllRows q

databaseOpen :: String -> IO Database
databaseOpen dbinfo = do
  c <- connectPostgreSQL' dbinfo
  _ <- sRun c "SET search_path = uzbl, public, global" []
  q <- mapM (newMVar <=< prepare c) queries
  return $ Database c $ listArray (minBound, maxBound) q

databaseClose :: Database -> IO ()
databaseClose = disconnect . databaseConnection
