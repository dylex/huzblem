module URIs
  ( uriDomain, inDomain, uriInDomain
  , expandURI
  ) where

import Control.Monad
import Data.Char
import Data.List
import qualified Data.Map as Map

import Network.URI

import Config

okInArg :: Char -> Bool
okInArg c = isAlphaNum c || c `elem` "!$'()*,/:"

uriDomain :: String -> Maybe String
uriDomain = fmap uriRegName . (uriAuthority <=< parseURI)

inDomain :: String -> String -> Bool
inDomain h d'@('.':d) = isSuffixOf d' h || h == d
inDomain h d = h == d || isSuffixOf ('.':d) h

uriInDomain :: String -> String -> Bool
uriInDomain u = maybe False (inDomain u) . uriDomain

escapeURIChar' :: (Char -> Bool) -> Char -> String
escapeURIChar' p ' ' | not (p ' ') = "+"
escapeURIChar' p c = escapeURIChar p c

escapeURIString' :: (Char -> Bool) -> String -> String
escapeURIString' = concatMap . escapeURIChar'

infixr 5 ?=
(?=) :: String -> String -> String
(?=) s = (++) s . escapeURIString' okInArg

rewrites :: Map.Map String (String -> String)
rewrites = Map.fromAscList
  [ ("amg",     ("http://www.allmusic.com/search/artist/" ?=))
  , ("dd",      ("https://duckduckgo.com/html/?kp=-1&q=" ?=))
  , ("dict",	("http://dictionary.reference.com/search?q=" ?=))
  , ("g",	("https://www.google.com/search?q=" ?=))
  , ("gi",	("https://www.google.com/search?tbm=isch&q=" ?=))
  , ("gm",      ("http://maps.google.com/maps?q=" ?=))
  , ("gn",	("http://news.google.com/news?q=" ?=))
  , ("gs",	("http://scholar.google.com/scholar?q=" ?=))
  , ("hdb",	("http://hackage.haskell.org/package/" ?=))
  , ("hoogle",	("http://haskell.org/hoogle/?q=" ?=))
  , ("imdb",	("http://imdb.com/find?q=" ?=))
  , ("math",	("http://mathworld.wolfram.com/search/?query=" ?=))
  , ("netflix", ("http://www.netflix.com/Search?v1=" ?=))
  , ("oed",	("http://127.0.0.1:31780/search?searchType=dictionary&q=" ?=))
  , ("om",	("https://openstreetmap.org/?query=" ?=))
  , ("rfc",	("http://tools.ietf.org/html/rfc" ?=)) -- "http://www.rfc-editor.org/rfc/rfc" ?= q ++ ".txt"
  , ("thes",	("http://thesaurus.reference.com/search?q=" ?=))
  , ("trackdhl", ("http://track.dhl-usa.com/TrackByNbr.asp?ShipmentNumber=" ?=))
  , ("trackfedex", ("http://www.fedex.com/cgi-bin/tracking?action=track&language=english&last_action=alttrack&ascend_header=1&cntry_code=us&initial=x&mps=y&tracknumbers=" ?=))
  , ("trackups", ("http://wwwapps.ups.com/etracking/tracking.cgi?TypeOfInquiryNumber=T&track.x=0&track.y=0&InquiryNumber1=" ?=))
  , ("trackusps", ("http://trkcnfrm1.smi.usps.com/PTSInternetWeb/InterLabelInquiry.do?origTrackNum=" ?=))
  , ("urban",	("https://www.urbandictionary.com/define.php?term=" ?=))
  , ("weather",	("http://classic.wunderground.com/cgi-bin/findweather/getForecast?query=" ?=))
  , ("wiki",	("https://www.wikipedia.org/w/wiki.phtml?search=" ?=))
  ]

aliases :: Map.Map String String
aliases = Map.fromAscList 
  [ ("amg",     "http://www.allmusic.com/")
  , ("gc",      "http://google.com/calendar")
  , ("gm",      "http://maps.google.com/")
  , ("hdb",	"http://hackage.haskell.org/packages")
  , ("imdb",	"http://imdb.com/")
  , ("math",	"http://mathworld.wolfram.com/")
  , ("netflix", "http://dvd.netflix.com/Queue")
  , ("om",	"http://openstreetmap.org/")
  , ("radar",	"http://classic.wunderground.com/radar/radblast.asp?num=10&delay=50&noclutter=0&ID=JFK&type=TR0&showstorms=0&lightning=0&showlabels=0&rainsnow=1")
  ]

defaultRewrite :: String -> String
defaultRewrite = rewrites Map.! "dd"

expandURI :: String -> String
expandURI "" = "about:blank"
expandURI (' ':s) = expandURI s
expandURI ('/':s) = "file://" ++ s
expandURI ('~':s@('/':_)) = "file://" ++ home ++ s
expandURI s = case find (`elem` ":. ") s of
  Just ':' -> s
  Just '.' -> "https://" ++ s
  _ -> case break (' '==) s of
    (k,' ':t) | Just r <- Map.lookup k rewrites -> r t
    (k,"") | Just r <- Map.lookup k aliases -> r
    _ -> defaultRewrite s
