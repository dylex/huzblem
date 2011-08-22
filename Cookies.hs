module Cookies 
  ( Cookies
  , emptyCookies
  , loadElinksCookies
  , cookiesArgs
  , argCookie
  ) where

import Control.Monad
import Data.Function
import Data.Maybe
import Data.Monoid
import qualified Data.Set as Set
import Data.Time.Clock.POSIX

import Util

data Cookie = Cookie
  { cookieDomain, cookiePath, cookieName, cookieValue :: String
  , cookieSecure :: Bool
  , cookieExpire :: Maybe POSIXTime
  } deriving (Eq, Show)

instance Ord Cookie where
  compare a b = mconcat $ map (\f -> on compare f a b) [cookieDomain, cookiePath, cookieName]

type Cookies = Set.Set Cookie

emptyCookies :: Cookies
emptyCookies = Set.empty

parseElinksCookie :: String -> Maybe Cookie
parseElinksCookie = pec . splitOn ('\t'==) where
  pec [name, value, _server, path, domain, expire, secure] = Just $ Cookie
    { cookieName = name
    , cookieValue = value
    , cookiePath = path
    , cookieDomain = '.':domain
    , cookieExpire = Just $ fromInteger $ read expire
    , cookieSecure = secure /= "0"
    }
  pec _ = Nothing

loadElinksCookies :: FilePath -> IO Cookies
loadElinksCookies f = Set.fromList . mapMaybe parseElinksCookie . lines =.< readFile f

cookieArgs :: Cookie -> [String]
cookieArgs c = [cookieDomain c, cookiePath c, cookieName c, cookieValue c, if cookieSecure c then "https" else "http", maybe "" ((show :: Integer -> String) . round) $ cookieExpire c]

cookiesArgs :: Cookies -> [[String]]
cookiesArgs = map cookieArgs . Set.toList

argCookie :: [String] -> Maybe Cookie
argCookie [domain,path,name,value,scheme,expire] = Just $ Cookie
  { cookieDomain = domain
  , cookiePath = path
  , cookieName = name
  , cookieValue = value
  , cookieSecure = scheme == "https"
  , cookieExpire = guard (not $ null expire) >. fromInteger (read expire)
  }
argCookie [domain,path,name,value,scheme] = argCookie [domain,path,name,value,scheme,""]
argCookie _ = Nothing
