{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module DomainMap 
  ( IsDomain
  , DomainMap
  , PM.empty
  , PM.null
  , singleton
  , insert
  , delete
  , lookup
  , lookupPrefix
  , foldrTree
  , toAscList
  , fromAscList
  , PM.partition
  ) where

import Prelude hiding (lookup, null)

import qualified Data.ByteString.Char8 as BS
import Data.String

import Safe

import Util
import qualified PrefixMap as PM

type Domain = [BS.ByteString]

class IsString s => IsDomain s where
  domain :: s -> Domain
  fromDomain :: Domain -> s

type DomainMap a = PM.PrefixMap BS.ByteString a

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

singleton :: IsDomain s => s -> a -> DomainMap a
singleton = PM.singleton . domain

insert :: IsDomain s => s -> a -> DomainMap a -> DomainMap a
insert = PM.insert . domain

delete :: IsDomain s => s -> DomainMap a -> DomainMap a
delete = PM.delete . domain

lookup :: IsDomain s => s -> DomainMap a -> Maybe a
lookup = PM.lookup . domain

lookupPrefix :: IsDomain s => s -> DomainMap a -> Maybe a
lookupPrefix = PM.lookupPrefix . domain

foldrTree :: (String -> b -> b -> b) -> (a -> b) -> b -> DomainMap a -> b
foldrTree f = PM.foldrTree (f . BS.unpack)

toAscList :: IsDomain s => DomainMap a -> [(s,a)]
toAscList = map (first fromDomain) . PM.toAscList

fromAscList :: IsDomain s => [(s,a)] -> DomainMap a
fromAscList = PM.fromAscList . map (first domain)
