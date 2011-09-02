module Scripts
  ( script
  , scriptLinkSelect
  , scriptActivate
  , scriptBlock
  , scriptKillScripts
  ) where

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

hostRegexp :: [String] -> String
hostRegexp l = "^https?://([^/?#]*\\.)?(" ++ intercalate "|" (map regexpQuote l) ++ ")([/?#]|$)"

type BlockList = (Bool, [String])

scriptBlock :: BlockList -> BlockList -> BlockList -> Script
scriptBlock scr ifr img = proc (
  " var uzbl_block = { \
  \   INPUT:{default:true}, \
  \   FRAME:{default:true}, \
  \   SCRIPT:{" ++ bv scr ++ "}, \
  \   IFRAME:{" ++ bv ifr ++ "}, \
  \   IMG:{" ++ bv img ++ "}, \
  \   verbose:true \
  \ };")
  ++ load "block"
  where
    bv (def, l) = "default:" ++ (if def then "true" else "false") ++ bl l
    bl [] = ""
    bl l = ",src:RegExp(" ++ string (hostRegexp l) ++ ",'i')"

scriptKillScripts :: Script
scriptKillScripts = load "killscript"
