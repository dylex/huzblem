module Cookies 
  ( Cookie
  , cookieDomain
  , Cookies
  , emptyCookies
  , loadCookies, loadElinksCookies
  , saveCookies
  , cookiesArgs
  , argCookie
  , cookieAdd
  ) where

import Control.Monad
import Data.Function
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Set as Set
import Data.Time.Clock.POSIX

import Safe

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
    { cookieDomain = '.':domain
    , cookiePath = path
    , cookieName = name
    , cookieValue = value
    , cookieSecure = secure /= "0"
    , cookieExpire = fmap fromInteger $ readMay expire
    }
  pec _ = Nothing

parseCookie :: String -> Maybe Cookie
parseCookie = pc . splitOn ('\t'==) where
  pc [domain, _flag, path, secure, expire, name, value] = Just $ Cookie
    { cookieDomain = domain
    , cookiePath = path
    , cookieName = name
    , cookieValue = value
    , cookieSecure = secure `notElem` ["FALSE","0"]
    , cookieExpire = fmap fromInteger $ readMay expire
    }
  pc _ = Nothing

argCookie :: [String] -> Maybe Cookie
argCookie [domain,path,name,value,scheme,expire] = Just $ Cookie
  { cookieDomain = domain
  , cookiePath = path
  , cookieName = name
  , cookieValue = value
  , cookieSecure = scheme == "https"
  , cookieExpire = guard (not $ null expire) >> readMay expire >.= fromInteger
  }
argCookie [domain,path,name,value,scheme] = argCookie [domain,path,name,value,scheme,""]
argCookie _ = Nothing

cookieExpires :: Cookie -> String
cookieExpires = maybe "" ((show :: Integer -> String) . round) . cookieExpire

writeCookie :: Cookie -> Maybe String
writeCookie Cookie{ cookieExpire = Nothing } = Nothing
writeCookie c = Just $ intercalate "\t" wc where
  wc = 
    [ cookieDomain c
    , tf (headMay (cookieDomain c) == Just '.')
    , cookiePath c
    , tf $ cookieSecure c
    , cookieExpires c 
    , cookieName c
    , cookieValue c
    ]
  tf False = "FALSE"
  tf True = "TRUE"

cookieArgs :: Cookie -> [String]
cookieArgs c = [cookieDomain c, cookiePath c, cookieName c, cookieValue c, if cookieSecure c then "https" else "http", cookieExpires c]

loadCookiesWith :: (String -> Maybe Cookie) -> FilePath -> IO [Cookie]
loadCookiesWith c = mapMaybe c . lines .=< readFile

loadElinksCookies :: FilePath -> IO Cookies
loadElinksCookies = Set.fromList .=< loadCookiesWith parseElinksCookie

loadCookies :: FilePath -> IO Cookies
loadCookies = Set.fromDistinctAscList .=< loadCookiesWith parseCookie

saveCookies :: FilePath -> Cookies -> IO ()
saveCookies f = writeFile f . unlines . mapMaybe writeCookie . Set.toAscList

cookiesArgs :: Cookies -> [[String]]
cookiesArgs = map cookieArgs . Set.toList

cookieAdd :: Cookie -> Cookies -> Cookies
cookieAdd = Set.insert
