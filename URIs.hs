module URIs
  ( uriDomain, inDomain, uriInDomain
  , expandURI
  ) where

import Data.Char
import Data.List
import qualified Data.Map as Map
import Data.Maybe

import Text.URI

import Config

okInArg :: Char -> Bool
okInArg c = isAlphaNum c || c `elem` "!$'()*,/:"

uriDomain :: String -> String
uriDomain u = fromMaybe "" $ uriRegName =<< parseURI u

inDomain :: String -> String -> Bool
inDomain h d'@('.':d) = isSuffixOf d' h || h == d
inDomain h d = h == d || isSuffixOf ('.':d) h

uriInDomain :: String -> String -> Bool
uriInDomain = inDomain . uriDomain

infixr 5 ?=
(?=) :: String -> String -> String
(?=) s = (++) s . escapeString okInArg

rewrites :: Map.Map String (String -> String)
rewrites = Map.fromAscList
  [ ("amg",     ("http://www.allmusic.com/search/artist/" ?=))
  , ("dd",      ("http://duckduckgo.com/?kp=-1&q=" ?=))
  , ("dict",	("http://dictionary.reference.com/search?q=" ?=))
  , ("fm",	("http://www.freshmeat.net/search/?q=" ?=))
  , ("g",	("http://www.google.com/search?q=" ?=))
  , ("gi",	("http://images.google.com/images?q=" ?=))
  , ("gn",	("http://news.google.com/news?q=" ?=))
  , ("gs",	("http://scholar.google.com/scholar?q=" ?=))
  , ("hdb",	("http://hackage.haskell.org/package/" ?=))
  , ("hoogle",	("http://haskell.org/hoogle/?q=" ?=))
  , ("imdb",	("http://imdb.com/Find?" ?=))
  , ("math",	("http://mathworld.wolfram.com/search/?query=" ?=))
  , ("netflix", ("http://www.netflix.com/Search?v1=" ?=))
  , ("oed",	("http://127.0.0.1:31780/search?searchType=dictionary&q=" ?=))
  , ("rfc",	\q -> "http://www.rfc-editor.org/rfc/rfc" ?= q ++ ".txt")
  , ("s",	("http://www.scroogle.org/cgi-bin/nbbw.cgi?Gw=" ?=))
  , ("thes",	("http://thesaurus.reference.com/search?q=" ?=))
  , ("trackdhl", ("http://track.dhl-usa.com/TrackByNbr.asp?ShipmentNumber=" ?=))
  , ("trackfedex", ("http://www.fedex.com/cgi-bin/tracking?action=track&language=english&last_action=alttrack&ascend_header=1&cntry_code=us&initial=x&mps=y&tracknumbers=" ?=))
  , ("trackups", ("http://wwwapps.ups.com/etracking/tracking.cgi?TypeOfInquiryNumber=T&track.x=0&track.y=0&InquiryNumber1=" ?=))
  , ("trackusps", ("http://trkcnfrm1.smi.usps.com/PTSInternetWeb/InterLabelInquiry.do?origTrackNum=" ?=))
  , ("urban",	("http://www.urbandictionary.com/define.php?term=" ?=))
  , ("weather",	("http://classic.wunderground.com/cgi-bin/findweather/getForecast?query=" ?=))
  , ("wiki",	("http://www.wikipedia.org/w/wiki.phtml?search=" ?=))
  ]

defaultRewrite :: String -> String
defaultRewrite = rewrites Map.! "s"

expandURI :: String -> String
expandURI "" = "about:blank"
expandURI (' ':s) = expandURI s
expandURI ('/':s) = "file://" ++ s
expandURI ('~':s@('/':_)) = "file://" ++ home ++ s
expandURI s = case find (`elem` ":. ") s of
  Just ':' -> s
  Just '.' -> "http://" ++ s
  _ -> case break (' '==) s of
    (k,' ':t) | Just r <- Map.lookup k rewrites -> r t
    _ -> defaultRewrite s
