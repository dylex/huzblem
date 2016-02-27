module Cookies 
  ( Cookie
  , cookieDomain
  , Cookies
  , emptyCookies
  , loadCookiesTxt, loadElinksCookies
  , saveCookiesTxt
  , cookiesArgs
  , argCookie
  , cookieAdd
  ) where

import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Function
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Set as Set
import Data.Time.Clock.POSIX

import Safe

import Util

data Cookie = Cookie
  { cookieDomain, cookiePath, cookieName, cookieValue :: BS.ByteString
  , cookieSecure :: Bool
  , cookieHttpOnly :: Bool
  , cookieExpire :: Maybe POSIXTime
  } deriving (Eq, Show)

instance Ord Cookie where
  compare a b = mconcat $ map (\f -> on compare f a b) [cookieDomain, cookiePath, cookieName]

type Cookies = Set.Set Cookie

emptyCookies :: Cookies
emptyCookies = Set.empty

parseElinksCookie :: String -> Maybe Cookie
parseElinksCookie = pec . splitOn ('\t'==) where
  pec [name, value, _server, path, domain, expire, secure] = Just Cookie
    { cookieDomain = BS.pack $ '.':domain
    , cookiePath = BS.pack path
    , cookieName = BS.pack name
    , cookieValue = BS.pack value
    , cookieSecure = secure /= "0"
    , cookieHttpOnly = False
    , cookieExpire = fmap fromInteger $ readMay expire
    }
  pec _ = Nothing

parseCookieTxt :: String -> Maybe Cookie
parseCookieTxt = pc . splitOn ('\t'==) where
  pc [domain, _flag, path, secure, expire, name, value] = Just Cookie
    { cookieDomain = BS.pack $ fromMaybe domain hod
    , cookiePath = BS.pack path
    , cookieName = BS.pack name
    , cookieValue = BS.pack value
    , cookieSecure = secure `notElem` ["FALSE","0"]
    , cookieHttpOnly = isJust hod
    , cookieExpire = fmap fromInteger $ readMay expire
    } where hod = stripPrefix "#HttpOnly_" domain
  pc _ = Nothing

argCookie :: [String] -> Maybe Cookie
argCookie [domain,path,name,value,scheme,expire] = Just Cookie
  { cookieDomain = BS.pack domain
  , cookiePath = BS.pack path
  , cookieName = BS.pack name
  , cookieValue = BS.pack value
  , cookieSecure = "https" `isPrefixOf` scheme
  , cookieHttpOnly = "Only" `isSuffixOf` scheme
  , cookieExpire = guard (not $ null expire) >> readMay expire >.= fromInteger
  }
argCookie [domain,path,name,value,scheme] = argCookie [domain,path,name,value,scheme,""]
argCookie _ = Nothing

cookieExpires :: Cookie -> String
cookieExpires = maybe "" ((show :: Integer -> String) . round) . cookieExpire

writeCookieTxt :: Cookie -> Maybe String
writeCookieTxt c = Just $ intercalate "\t"
  [ (guard (cookieHttpOnly c) >> "#HttpOnly_") ++ BS.unpack (cookieDomain c)
  , tf $ not (BS.null (cookieDomain c)) && BS.head (cookieDomain c) == '.'
  , BS.unpack $ cookiePath c
  , tf $ cookieSecure c
  , cookieExpires c 
  , BS.unpack $ cookieName c
  , BS.unpack $ cookieValue c
  ] where
  tf False = "FALSE"
  tf True = "TRUE"

cookieArgs :: Cookie -> [String]
cookieArgs c = 
  [ BS.unpack $ cookieDomain c
  , BS.unpack $ cookiePath c
  , BS.unpack $ cookieName c
  , BS.unpack $ cookieValue c
  , (if cookieSecure c then "https" else "http") ++ (guard (cookieHttpOnly c) >> "Only")
  , cookieExpires c]

guardExpired :: POSIXTime -> Cookie -> Maybe Cookie
guardExpired now c@Cookie{ cookieExpire = Just t } | t > now = Just c
guardExpired _ _ = Nothing

loadCookiesWith :: (String -> Maybe Cookie) -> FilePath -> IO [Cookie]
loadCookiesWith c f = do
  now <- getPOSIXTime
  mapMaybe (guardExpired now <=< c) . lines =.< readFile f

loadElinksCookies :: FilePath -> IO Cookies
loadElinksCookies = Set.fromList .=< loadCookiesWith parseElinksCookie

loadCookiesTxt :: FilePath -> IO Cookies
loadCookiesTxt = Set.fromDistinctAscList .=< loadCookiesWith parseCookieTxt

saveCookiesTxt :: FilePath -> Cookies -> IO ()
saveCookiesTxt f c = do
  now <- getPOSIXTime
  writeFile f $ unlines $ mapMaybe (writeCookieTxt <=< guardExpired now) $ Set.toAscList c

cookiesArgs :: Cookies -> [[String]]
cookiesArgs = map cookieArgs . Set.toList

cookieAdd :: Cookie -> Cookies -> Cookies
cookieAdd = Set.insert
