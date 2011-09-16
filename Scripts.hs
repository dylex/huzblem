module Scripts
  ( script, Script
  , scriptRequest
  , scriptInit
  , scriptSetDomain
  , scriptLinkSelect
  , scriptActivate
  , scriptLinkGet
  , scriptLinkNumber
  , scriptFocus
  , scriptSetBlocks
  , scriptSetBlock
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

import Block
import Config
import Util
import Keys
import qualified DomainMap as DM

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

hostRegexp :: String -> String
hostRegexp h = regexp ("^https?://(?:[^/?#]*\\.)?(" ++ h ++ ")\\.?(?::\\d+)?(?:[/?#]|$)") "i"

data Count a
  = None
  | One { unCount :: !a }
  | Group { unCount :: !a }

domainRegexp :: DM.DomainMap a -> String
domainRegexp t
  | DM.null t = "false"
  | otherwise = hostRegexp dr where
  dr = unCount $ DM.foldrTree df (const None) None t
  df n d = alt $ sub d ++ regexpQuote n
  sub None = ""
  sub (One d) = d ++ "\\."
  sub (Group d) = "(?:" ++ d ++ ")\\."
  alt x None = One x
  alt x r = Group $ x ++ '|' : unCount r

load :: FilePath -> Script
load = sq . Unsafe.unsafeDupablePerformIO . readFile . uzblHome . (<.>"js")

script :: Script -> String
script = ("js " ++) . (++ "undefined") . escape

scriptRequest :: String -> String -> Script -> String
scriptRequest i t a = "js " ++ string ("REQUEST [" ++ i ++ "] " ++ t ++ " ") ++ "+(" ++ escape a ++ ")"

scriptInit :: Script
scriptInit = load "huzbl" ++ load "block" ++ load "linknumber"

scriptSetDomain :: String -> Script
scriptSetDomain dom = "huzbl.domainre=" ++ hostRegexp (regexpQuote dom) ++ ";"

scriptLinkSelect :: String -> Maybe String -> Script
scriptLinkSelect t r = "huzbl.linkSelect(" ++ string t ++ ", " ++ regexp (fromMaybe ("\\b" ++ regexpQuote t) r) "i" ++ ");"

scriptActivate :: Maybe Int -> Script
scriptActivate Nothing = "huzbl.activate(document.activeElement);"
scriptActivate (Just n) = "huzbl.linkNumber.activate(" ++ show n ++ ");"

scriptLinkGet :: Maybe Int -> Script
scriptLinkGet Nothing = "document.activeElement.href" -- FIXME
scriptLinkGet (Just n) = "huzbl.linkNumber.get(" ++ show n ++ ")"

scriptFocus :: Int -> Script
scriptFocus n = "huzbl.linkNumber.focus(" ++ show n ++ ");"

scriptLinkNumber :: Bool -> Script
scriptLinkNumber y = "huzbl.linkNumber." ++ (if y then "show" else "hide") ++ "();"

scriptSetBlocks :: Blocks -> Script
scriptSetBlocks bl =
  "huzbl.blockre=" ++ br ++ ";huzbl.trustre=" ++ tr ++ ";"
  where (tr, br) = both domainRegexp $ DM.partition id bl

scriptSetBlock :: Bool -> [(String, BlockMode)] -> Script
scriptSetBlock verb bm =
  "huzbl.block={" ++ concatMap bf bm ++ "verbose:" ++ bool verb ++ "};"
  where
    bf (t,m) = map toUpper t ++ ":{default:" ++ bv m ++ "},"
    bv m = bool (blockModeDefault m) 
      ++ (guard (blockModeCurrent m) >> ",cur:true")
      ++ (guard (blockModeList m) >> ",src:" ++ if blockModeDefault m then "huzbl.blockre" else "huzbl.trustre")

scriptKillScripts :: Script
scriptKillScripts = load "killscript"

scriptKeydown :: ModKey -> Script
scriptKeydown (m,k) =
  "var event=document.createEvent('KeyboardEvent');\
  \event.initKeyboardEvent('keydown',false,false,null," ++ string k ++ ",0" ++ concatMap b ["Ctrl","Alt","Shift","Mod1"] ++ ",false);\
  \document.dispatchEvent(event);"
  where b = (',':) . bool . (`modifierTest` m) 
