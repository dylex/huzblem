{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module DomainMap 
  ( IsDomain
  , DomainMap
  , empty
  , null
  , singleton
  , insert
  , delete
  , lookup
  , lookupPrefix
  , partition
  , foldrTree
  , toAscList
  , fromAscList
  ) where

import Prelude hiding (lookup, null)

import qualified Data.ByteString.Char8 as BS
import Data.Function
import qualified Data.List as List
import qualified Data.Map as M
import Data.String

import Safe

import Util

type Domain = [BS.ByteString]

class IsString s => IsDomain s where
  domain :: s -> Domain
  fromDomain :: Domain -> s

data DomainMap a
  = Leaf a
  | Node (M.Map BS.ByteString (DomainMap a))
  deriving (Show)

instance IsDomain BS.ByteString where
  domain s
    | BS.null s = []
    | BS.last s == '.' = spl $ BS.init s
    | otherwise = spl s
    where spl i = n : domain h where (h,n) = BS.breakEnd ('.'==) i
  fromDomain = BS.intercalate (BS.singleton '.') . reverse

instance IsDomain String where
  -- domain = domain . BS.pack
  domain s = spl s [] where
    spl "" d = d
    spl i d = spl (tailSafe r) (BS.pack n : d) where (n,r) = break ('.'==) i
  fromDomain = BS.unpack . fromDomain

instance Functor DomainMap where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node m) = Node $ fmap (fmap f) m

-- this may only appear at the top
empty :: DomainMap a
empty = Node M.empty

null :: DomainMap a -> Bool
null (Node m) = M.null m
null _ = False

norm :: DomainMap a -> Maybe (DomainMap a)
norm (Node m) | M.null m = Nothing
norm d = Just d

singleton' :: Domain -> a -> DomainMap a
singleton' [] = Leaf
singleton' (n:d) = Node . M.singleton n . singleton' d

singleton :: IsDomain s => s -> a -> DomainMap a
singleton = singleton' . domain

insert' :: Domain -> a -> DomainMap a -> DomainMap a
insert' [] x _ = Leaf x
insert' d x (Leaf _) = singleton' d x
insert' (n:d) x (Node m) = Node $ M.insertWith (\_ -> insert' d x) n (singleton' d x) m

insert :: IsDomain s => s -> a -> DomainMap a -> DomainMap a
insert = insert' . domain

delete' :: Domain -> DomainMap a -> DomainMap a
delete' [] (Leaf _) = empty
delete' (n:d) (Node m) = Node $ M.update (norm . delete' d) n m
delete' _ d = d

delete :: IsDomain s => s -> DomainMap a -> DomainMap a
delete = delete' . domain

lookup' :: Domain -> DomainMap a -> Maybe a
lookup' [] (Leaf x) = Just x
lookup' (n:d) (Node m) = M.lookup n m >>= lookup' d
lookup' _ _ = Nothing

lookup :: IsDomain s => s -> DomainMap a -> Maybe a
lookup = lookup' . domain

lookupPrefix' :: Domain -> DomainMap a -> Maybe a
lookupPrefix' _ (Leaf x) = Just x
lookupPrefix' (n:d) (Node m) = M.lookup n m >>= lookupPrefix' d
lookupPrefix' _ _ = Nothing

lookupPrefix :: IsDomain s => s -> DomainMap a -> Maybe a
lookupPrefix = lookupPrefix' . domain

partition :: (a -> Bool) -> DomainMap a -> (DomainMap a, DomainMap a)
partition p (Leaf a) 
  | p a = (Leaf a, empty)
  | otherwise = (empty, Leaf a)
partition p (Node m) = (join l, join r) where
  (l,r) = unzip $ map part $ M.toAscList m
  part (n,d) = both ((,) n) $ partition p d
  join = Node . M.fromAscList . filter (not . null . snd)

foldrTree' :: (BS.ByteString -> b -> b -> b) -> (a -> b) -> b -> DomainMap a -> b
foldrTree' _ fl _ (Leaf x) = fl x
foldrTree' fn fl b (Node m) = M.foldrWithKey (\n -> fn n . foldrTree' fn fl b) b m

foldrTree :: (String -> b -> b -> b) -> (a -> b) -> b -> DomainMap a -> b
foldrTree f = foldrTree' (f . BS.unpack)

toAscList' :: DomainMap a -> [(Domain,a)]
toAscList' (Leaf x) = [([],x)]
toAscList' (Node m) = concatMap tal $ M.toAscList m where
  tal (n,d) = map (first (n:)) $ toAscList' d

toAscList :: IsDomain s => DomainMap a -> [(s,a)]
toAscList = map (first fromDomain) . toAscList'

fromAscList' :: [(Domain,a)] -> DomainMap a
fromAscList' [] = empty
fromAscList' [(d,x)] = singleton' d x
fromAscList' l = Node $ M.fromAscList [ (n, fromAscList' $ map (first tail) ln) 
  | ln@((n:_,_):_) <- List.groupBy ((==) `on` (headNote "DomainMap.fromAscList: invalid" . fst)) l ]

fromAscList :: IsDomain s => [(s,a)] -> DomainMap a
fromAscList = fromAscList' . map (first domain)
