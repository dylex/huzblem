{-# LANGUAGE ScopedTypeVariables #-}
module Event
  ( event
  ) where

import Prelude hiding (log)

import Control.Monad
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Numeric

import Safe

import Util
import Config
import Keys
import Uzbl
import Bind
import Cookies
import Database
import Scripts
import URIs

badArgs :: UzblM ()
badArgs = log "unknown arguments"

commandError :: [String] -> UzblM ()
commandError [_] = log ""
commandError _ = badArgs

commandExecuted :: [String] -> UzblM ()
commandExecuted ("add_cookie":args) = maybe badArgs ac $ argCookie args where
  ac c = modify $ \u -> u{ uzblCookies = cookieAdd c (uzblCookies u) }
commandExecuted _ = nop

variableSet :: [String] -> UzblM ()
variableSet ["inject_html",_,_] = nop
variableSet [var,typ,sval] | Just val <- readValue typ sval = do
  setVar' var val
  when ("block_" `isPrefixOf` var) updateBlockScript
variableSet _ = badArgs

fifoSet :: [String] -> UzblM ()
fifoSet [fifo] = modify $ \u -> u { uzblFIFO = Just fifo }
fifoSet _ = badArgs

socketSet :: [String] -> UzblM ()
socketSet [sock] = modify $ \u -> u { uzblSocket = Just sock }
socketSet _ = badArgs

keyPress :: [String] -> UzblM ()
keyPress [md,key] = flip runBind (readModifiers md, key) . uzblBindings =<< get 
keyPress _ = badArgs

newWindow :: [String] -> UzblM ()
newWindow [u] = goto u
newWindow _ = badArgs

allow :: String -> String -> UzblM Bool
allow bt dom = do
  (d, l) <- blockMode . toEnum =.< getVarInt ("block_" ++ bt)
  fromMaybe d =.< if l
    then withDatabase $ blockTest dom
    else return Nothing

acceptCookie :: Cookie -> UzblM Bool
acceptCookie c = allow "cookie" (cookieDomain c)

addCookie :: [String] -> UzblM ()
addCookie args = maybe badArgs ac $ argCookie args where
  ac c = do
    ok <- acceptCookie c
    if ok
      then do
        log "accepting"
        modify $ \u -> u{ uzblCookies = cookieAdd c (uzblCookies u) }
      else do
        log "rejecting"
        runArgs "delete_cookie" args

loadStart :: [String] -> UzblM ()
loadStart [u] = do
  setVar' "uri" (ValStr u) -- fake it here, since we don't get the event otherwise
  setVar' "TITLE" ValNone
  setVar' "SELECTED_URI" ValNone
  setVar "status_load" $ ValStr "wait"
  status ""
loadStart _ = badArgs

loadCommit :: [String] -> UzblM ()
loadCommit [u] = do
  b <- uzblBlockScript =.< get
  as <- allow "script" (uriDomain u)
  run $ script $ b ++ if as then "" else scriptKillScripts
  loadStart [u] -- sometimes we don't get this event?
  setVar "status_load" $ ValStr "recv"
loadCommit _ = badArgs

loadFinish :: [String] -> UzblM ()
loadFinish [u] = do
  setVar "status_load" $ ValStr "" -- "done"
  t <- getVar "TITLE"
  p <- getVarInt "enable_private"
  unless (p /= 0 || "file:///" `isPrefixOf` u || "about:" `isPrefixOf` u) $
    withDatabase $ browseAdd u $ case t of { ValStr s -> Just s ; _ -> Nothing }
loadFinish _ = badArgs

loadProgress :: [String] -> UzblM ()
loadProgress [sp] 
  | Just (p :: Int) <- readMay sp
  , 0 <= p && p <= 100 =
  setVar "status_load_color" $ ValStr $ '#' : sc (100-p) (sc p "00")
  where
    sh x 
      | x < 16 = ('0':) . h
      | otherwise = h
      where h = showHex x
    sc = sh . (`div`100) . (255*)
loadProgress _ = badArgs

titleChanged :: [String] -> UzblM ()
titleChanged ["(no title)"] = do
  setVar' "TITLE" ValNone
titleChanged [t] = do
  u <- uzblURI
  setVar' "TITLE" (ValStr t) 
  withDatabase $ browseSetTitle u t
titleChanged _ = badArgs

linkHover :: [String] -> UzblM ()
linkHover [u] = setVar' "SELECTED_URI" $ ValStr u
linkHover _ = badArgs

linkUnHover :: [String] -> UzblM ()
linkUnHover _ = setVar' "SELECTED_URI" ValNone

downloadComplete :: [String] -> UzblM ()
downloadComplete [f] = io $ putStrLn $ "download complete: " ++ f
downloadComplete _ = badArgs

events :: Map.Map Event ([String] -> UzblM ())
events = Map.fromAscList $ map (first Event) $ 
  [ ("ADD_COOKIE",	addCookie)
  , ("COMMAND_ERROR",	commandError)
  , ("COMMAND_EXECUTED",commandExecuted)
  , ("DOWNLOAD_COMPLETE",downloadComplete)
  , ("FIFO_SET",	fifoSet)
  , ("FORM_ACTIVE",	\_ -> rawMode)
  , ("KEY_PRESS",	keyPress)
  , ("LINK_HOVER",	linkHover)
  , ("LINK_UNHOVER",	linkUnHover)
  , ("LOAD_COMMIT",	loadCommit)
  , ("LOAD_FINISH",	loadFinish)
  , ("LOAD_PROGRESS",	loadProgress)
  , ("LOAD_START",	loadStart)
  , ("NEW_WINDOW",	newWindow)
  , ("SOCKET_SET",	socketSet) 
  , ("TITLE_CHANGED",	titleChanged) 
  , ("VARIABLE_SET",	variableSet) 
  ]

event :: Event -> [String] -> UzblM ()
event ev args = maybe nop
  ($ args) $ Map.lookup ev events
