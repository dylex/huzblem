module Scripts
  ( script, Script
  , scriptHuzbl
  , scriptSetDomain
  , scriptLinkSelect
  , scriptActivate
  , scriptBlock
  , scriptKillScripts
  , scriptKeydown
  ) where

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import System.FilePath
import qualified System.IO.Unsafe as Unsafe

import Safe

import Config
import Util
import Keys

type Script = String

data ProcMode
  = ProcSym
  | ProcWord
  | ProcQuote !Char
  deriving (Eq)

sq :: Script -> Script
--proc = unwords . words
sq = flip p ProcSym where
  p [] (ProcQuote _) = error "unterminated quote in script"
  p [] _ = []
  p ('\\':c:s) m@(ProcQuote q) | c == q || c == '\\' = '\\':c:p s m
  p ('\'':s) m = '\'':pq '\'' s m
  p ('\"':s) m = '\"':pq '\"' s m
  p (c:s) m 
    | isSpace c = case m of
      ProcWord | isAlphaNum (headDef ' ' s) -> ' ':p s ProcSym
      ProcQuote _ 
        | c /= '\n' && c /= '\r' -> c:p s m
        | otherwise -> error "newline in script quote"
      _ -> p s m
    | otherwise = c:p s (case m of
      ProcQuote _ -> m
      _ | isAlphaNum c -> ProcWord 
        | otherwise -> ProcSym)
  pq c s m@(ProcQuote q)
    | q == c = p s ProcSym
    | otherwise = p s m
  pq c s _ = p s (ProcQuote c)

string :: String -> Script
string = show -- close enough

bool :: Bool -> Script
bool False = "false"
bool True = "true"

regexpQuote :: String -> String
regexpQuote "" = ""
regexpQuote (c:s)
  | c `elem` "$()*+.?[\\]^{|}" = '\\':s'
  | otherwise = s'
  where s' = c:regexpQuote s

regexp :: String -> String -> Script
regexp i p = '/' : r i ++ '/' : p where
  r [] = []
  r ('/':s) = '\\':'/':r s
  r (c:s) = c:r s

-- this function could be made much better (and may assume the list is domain-grouped)
hostRegexp :: [String] -> String
hostRegexp l = regexp ("^https?://([^/?#]*\\.)?(" ++ intercalate "|" (map regexpQuote l) ++ ")([/?#]|$)") "i"

load :: FilePath -> Script
load = sq . Unsafe.unsafeDupablePerformIO . readFile . uzblHome . (<.>"js")

script :: Script -> String
script = ("js " ++) . (++ "undefined") . escape

scriptHuzbl :: Script
scriptHuzbl = load "huzbl"

scriptSetDomain :: String -> Script
scriptSetDomain dom = "huzbl.domainre=" ++ hostRegexp [dom] ++ ";"

scriptLinkSelect :: String -> Maybe String -> Script
scriptLinkSelect t r = "huzbl.linkSelect(" ++ string t ++ ", " ++ regexp (fromMaybe ("\\b" ++ regexpQuote t) r) "i" ++ ");"

scriptActivate :: Script
scriptActivate = "huzbl.activate(document.activeElement);"

scriptBlock :: Bool -> ([String], [String]) -> [(String, BlockMode)] -> Script
scriptBlock verb (bl,tl) bm =
  "huzbl.block={" ++ concatMap bf bm ++ "verbose:" ++ bool verb ++ "};" ++ load "block"
  where
    bf (t,m) = map toUpper t ++ ":{default:" ++ bv m ++ "},"
    bv m = bool (blockModeDefault m) 
      ++ (guard (blockModeList m) >> br (if blockModeDefault m then bl else tl)) 
      ++ (guard (m == AllowTrustedCurrent) >> ",cur:true")
    br [] = ""
    br l = ",src:" ++ hostRegexp l

scriptKillScripts :: Script
scriptKillScripts = load "killscript"

scriptKeydown :: ModKey -> Script
scriptKeydown (m,k) =
  "var event=document.createEvent('KeyboardEvent');\
  \event.initKeyboardEvent('keydown',false,false,null," ++ string k ++ ",0" ++ concatMap b ["Ctrl","Alt","Shift","Mod1"] ++ ",false);\
  \document.dispatchEvent(event);"
  where b = (',':) . bool . (`modifierTest` m) 
