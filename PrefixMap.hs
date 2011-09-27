module PrefixMap
  ( PrefixMap
  , empty
  , null
  , singleton
  , insert
  , delete
  , alter
  , lookup
  , lookupPrefix
  , partition
  , foldrTree
  , toAscList
  , fromAscList
  ) where

import Prelude hiding (lookup, null)
import Data.Function
import qualified Data.List as List
import qualified Data.Map as M
import Data.Maybe

import Safe

import Util

-- |A map in which each key is a list and no key is a prefix of any other key.
data PrefixMap k a
  = Leaf a
  | Node (M.Map k (PrefixMap k a))
  deriving (Show)

instance Functor (PrefixMap k) where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node m) = Node $ fmap (fmap f) m

-- this may only appear at the top
empty :: PrefixMap k a
empty = Node M.empty

null :: PrefixMap k a -> Bool
null (Node m) = M.null m
null _ = False

nonull :: PrefixMap k a -> Maybe (PrefixMap k a)
nonull (Node m) | M.null m = Nothing
nonull d = Just d

renull :: Maybe (PrefixMap k a) -> PrefixMap k a
renull = fromMaybe empty

singleton :: [k] -> a -> PrefixMap k a
singleton [] = Leaf
singleton (n:d) = Node . M.singleton n . singleton d

-- |Insert a new entry to the map.  Any existing conflicting entries (prefixes or elongations of the given key) are replaced.
insert :: Ord k => [k] -> a -> PrefixMap k a -> PrefixMap k a
insert [] x _ = Leaf x
insert d x (Leaf _) = singleton d x
insert (n:d) x (Node m) = Node $ M.insertWith (\_ -> insert d x) n (singleton d x) m

-- |Remove a single entry from the map, if present.
delete :: Ord k => [k] -> PrefixMap k a -> PrefixMap k a
delete [] (Leaf _) = empty
delete (n:d) (Node m) = Node $ M.update (nonull . delete d) n m
delete _ d = d

-- |Modify, insert, or delete the value associated with a key.  Note that when the supplied update function is 'id' the map is not modified in any way and specifically conflicting entries are preserved.
alter :: Ord k => (Maybe a -> Maybe a) -> [k] -> PrefixMap k a -> PrefixMap k a
alter f [] (Leaf x)
  | Just y <- f (Just x) = Leaf y
  | otherwise = empty
alter f (n:k) (Node m) = Node $ M.alter (nonull . alter f k . renull) n m
alter f k t
  | Just y <- f Nothing = singleton k y
  | otherwise = t -- empty?

-- |Lookup an exact key in the map.
lookup :: Ord k => [k] -> PrefixMap k a -> Maybe a
lookup [] (Leaf x) = Just x
lookup (n:d) (Node m) = M.lookup n m >>= lookup d
lookup _ _ = Nothing

-- |Lookup a key or any prefix of that key.
lookupPrefix :: Ord k => [k] -> PrefixMap k a -> Maybe a
lookupPrefix _ (Leaf x) = Just x
lookupPrefix (n:d) (Node m) = M.lookup n m >>= lookupPrefix d
lookupPrefix _ _ = Nothing

partition :: (a -> Bool) -> PrefixMap k a -> (PrefixMap k a, PrefixMap k a)
partition p (Leaf a) 
  | p a = (Leaf a, empty)
  | otherwise = (empty, Leaf a)
partition p (Node m) = (join l, join r) where
  (l,r) = unzip $ map part $ M.toAscList m
  part (n,d) = both ((,) n) $ partition p d
  join = Node . M.fromDistinctAscList . filter (not . null . snd)

foldrTree :: (k -> b -> b -> b) -> (a -> b) -> b -> PrefixMap k a -> b
foldrTree _ fl _ (Leaf x) = fl x
foldrTree fn fl b (Node m) = M.foldrWithKey (\n -> fn n . foldrTree fn fl b) b m

toAscList :: PrefixMap k a -> [([k],a)]
toAscList (Leaf x) = [([],x)]
toAscList (Node m) = concatMap tal $ M.toAscList m where
  tal (n,d) = map (first (n:)) $ toAscList d

fromAscList :: Eq k => [([k],a)] -> PrefixMap k a
fromAscList [] = empty
fromAscList [(d,x)] = singleton d x
fromAscList l = Node $ M.fromDistinctAscList [ (n, fromAscList $ map (first tail) ln) 
  | ln@((n:_,_):_) <- List.groupBy ((==) `on` (headNote "PrefixMap.fromAscList: invalid" . fst)) l ]
