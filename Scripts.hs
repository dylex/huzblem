module Scripts
  ( script, Script
  , scriptLinkSelect
  , scriptActivate
  , scriptBlock
  , scriptKillScripts
  ) where

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import System.FilePath
import qualified System.IO.Unsafe as Unsafe

import Config
import Util

type Script = String

proc :: Script -> Script
proc = escape . unwords . words

string :: String -> Script
string = show -- close enough

bool :: Bool -> Script
bool False = "false"
bool True = "true"

load :: FilePath -> Script
load = proc . Unsafe.unsafeDupablePerformIO . readFile . uzblHome . (<.>"js")

script :: Script -> String
script = ("js " ++)

scriptLinkSelect :: String -> Maybe String -> Script
scriptLinkSelect t r = proc $
  " var el = document.querySelector('link[rel=" ++ string t ++ "]'); \
  \ if (el) \
  \   location = el.href; \
  \ else { \
  \   var r = RegExp(" ++ string (fromMaybe ("\\b" ++ t) r) ++ ", 'i'); \
  \   var els = document.getElementsByTagName('a'); \
  \   for (var i = 0; i < els.length; ++i) \
  \     if (r.test(els[i].text)) { \
  \       els[i].focus(); \
  \       break; \
  \     } \
  \ } \
  \ undefined;"

scriptActivate :: Script
scriptActivate = load "activate"

regexpQuote :: String -> String
regexpQuote "" = ""
regexpQuote (c:s)
  | c `elem` "$()*+.?[\\]^{|}" = '\\':s'
  | otherwise = s'
  where s' = c:regexpQuote s

-- this function could be made much better (and may assume the list is domain-ordered)
hostRegexp :: [String] -> String
hostRegexp l = "^https?://([^/?#]*\\.)?(" ++ intercalate "|" (map regexpQuote l) ++ ")([/?#]|$)"

scriptBlock :: Bool -> ([String], [String]) -> [(String, BlockMode)] -> Script
scriptBlock verb (bl,tl) bm =
  proc ("var uzbl_block={" ++ concatMap bf bm ++ "verbose:" ++ bool verb ++ "};") ++ load "block"
  where
    bf (t,m) = map toUpper t ++ ":{default:" ++ bv (blockMode m) ++ "},"
    bv (b,l) = bool b ++ br (guard l >> if b then bl else tl)
    br [] = ""
    br l = ",src:RegExp(" ++ string (hostRegexp l) ++ ",'i')"

scriptKillScripts :: Script
scriptKillScripts = load "killscript"
