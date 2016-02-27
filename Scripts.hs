module Scripts
  ( scriptRequest
  , scriptSetDomain
  , scriptLinkSelect
  , scriptActivate
  , scriptLinkGet
  , scriptLinkNumber
  , scriptFocus
  , scriptSetBlocks
  , scriptSetBlock
  , scriptKeydown
  , scriptImprobableIslandKey
  ) where

import Control.Monad
import Data.Char
import Data.Maybe
import qualified Data.Time

import Block
import Util
import Keys
import qualified DomainMap as DM

string :: String -> String
string = show -- close enough

bool :: Bool -> String
bool False = "false"
bool True = "true"

regexpQuote :: String -> String
regexpQuote "" = ""
regexpQuote (c:s)
  | c `elem` "$()*+.?[\\]^{|}" = '\\':s'
  | otherwise = s'
  where s' = c:regexpQuote s

regexp :: String -> String -> String
regexp i p = '/' : r i where
  r [] = '/' : p
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

scriptRequest :: String -> String -> String -> String
scriptRequest i t a = string ("REQUEST [" ++ i ++ "] " ++ escape t ++ " ") ++ "+(" ++ a ++ ")"

scriptSetDomain :: Maybe String -> String
scriptSetDomain (Just dom) = "huzbl.domainre=" ++ hostRegexp (regexpQuote dom) ++ ";"
scriptSetDomain Nothing = "huzbl.domainre=false;"

scriptLinkSelect :: String -> Maybe String -> String
scriptLinkSelect t r = "huzbl.linkSelect(" ++ string t ++ ", " ++ regexp (fromMaybe ("\\b" ++ regexpQuote t) r) "i" ++ ");"

scriptActivate :: Maybe Int -> String
scriptActivate Nothing = "huzbl.activate(document.activeElement);"
scriptActivate (Just n) = "huzbl.linkNumber.activate(" ++ show n ++ ");"

scriptLinkGet :: Maybe Int -> String
scriptLinkGet Nothing = "document.activeElement.href" -- FIXME
scriptLinkGet (Just n) = "huzbl.linkNumber.get(" ++ show n ++ ")"

scriptFocus :: Int -> String
scriptFocus n = "huzbl.linkNumber.focus(" ++ show n ++ ");"

scriptLinkNumber :: Bool -> String
scriptLinkNumber y = "huzbl.linkNumber." ++ (if y then "show" else "hide") ++ "();"

scriptSetBlocks :: Blocks -> String
scriptSetBlocks bl =
  "huzbl.blockre=" ++ br ++ ";huzbl.trustre=" ++ tr ++ ";"
  where (tr, br) = both domainRegexp $ DM.partition id bl

scriptSetBlock :: Bool -> [(String, BlockMode)] -> String
scriptSetBlock verb bm =
  "huzbl.block={" ++ concatMap bf bm ++ "verbose:" ++ bool verb ++ "};"
  where
    bf (t,m) = map toUpper t ++ ":{default:" ++ bv m ++ "},"
    bv m = bool (blockModeDefault m) 
      ++ (guard (blockModeCurrent m) >> ",cur:true")
      ++ (guard (blockModeList m) >> ",src:" ++ if blockModeDefault m then "huzbl.blockre" else "huzbl.trustre")

scriptKeydown :: ModKey -> String
scriptKeydown (m,k) =
  "var event=document.createEvent('KeyboardEvent');\
  \event.initKeyboardEvent('keydown',false,false,null," ++ string k ++ ",0" ++ concatMap b ["Ctrl","Alt","Shift","Mod1"] ++ ",false);\
  \document.dispatchEvent(event);"
  where b = (',':) . bool . (`modifierTest` m) 

scriptImprobableIslandKey :: Bool -> Data.Time.NominalDiffTime -> Char -> String
scriptImprobableIslandKey p t k = 
  "var iitre=/^(?:Repeater: )?(?:([0-9.]+) Seconds? (early|late) \\\\/ |Perfect!(?: \\\\/ [0-9]+-chain!(?: \\\\/ New Personal Chain Record!)?)? \\\\/ )?Next target: ([2-9]) seconds$/;\
  \var iitm=Array.prototype.filter.call(document.getElementsByTagName('td'),function(e){return iitre.test(e.innerText)});\
  \if(iitm.length!==1)window.alert('No timing ('+iitm.length+')');\
  \else{\
    \var iitr=iitre.exec(iitm[0].innerText);\
    \var iit=parseInt(iitr[3]);\
    \var iito=0;" ++
    (if p then "if(iitr[2]==='late')\
      \iito=parseFloat(iitr[1]);" else "") ++
    "window.setTimeout(function(){\
      \keyevent({charCode:" ++ show (ord k) ++ ",altKey:0,ctrlKey:0,metaKey:0,originalTarget:document});\
    \}, 1000*(iit-(" ++ init (show t) ++ "+iit-iito)%iit));\
  \}"
