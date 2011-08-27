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
import Text.URI

import Util
import Config
import Keys
import Uzbl
import Bind
import Cookies
import Database

badArgs :: UzblM ()
badArgs = log "unknown arguments"

commandError :: [String] -> UzblM ()
commandError [_] = log ""
commandError _ = badArgs

variableSet :: [String] -> UzblM ()
variableSet [var,typ,sval] | Just val <- readValue typ sval =
  modify $ \u -> u{ uzblVariables = Map.insert var val (uzblVariables u) }
variableSet _ = badArgs

fifoSet :: [String] -> UzblM ()
fifoSet [fifo] = modify $ \u -> u { uzblFIFO = Just fifo }
fifoSet _ = badArgs

socketSet :: [String] -> UzblM ()
socketSet [sock] = modify $ \u -> u { uzblSocket = Just sock }
socketSet _ = badArgs

keyPress :: [String] -> UzblM ()
keyPress [md,key] = ($ (readModifiers md, key)) . uzblBind =<< get 
keyPress _ = badArgs

newWindow :: [String] -> UzblM ()
newWindow [u] = goto u
newWindow _ = badArgs

acceptCookie :: Cookie -> UzblM Bool
acceptCookie c = do
  cm <- getVarInt "cookie_mode"
  case cm of
    2 -> return True
    1 -> do
      u <- uzblURI -- should be new URI, possibly unknown
      let dom = fromMaybe "" $ (uriRegName <=< parseURI) u
      return $ cookieDomain c `isSuffixOf` dom || cookieDomain c == '.':dom
    _ -> return False

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
        run "delete_cookie" args

loadStart :: [String] -> UzblM ()
loadStart [u] = do
  variableSet ["uri","str",u] -- fake it here, since we don't get the event otherwise
  setVar "status_load" $ ValStr "wait"
loadStart _ = badArgs

loadCommit :: [String] -> UzblM ()
loadCommit [_] =
  setVar "status_load" $ ValStr "recv"
loadCommit _ = badArgs

loadFinish :: [String] -> UzblM ()
loadFinish [u] = do
  setVar "status_load" $ ValStr ""
  io . browseAdd u . uzblDatabase . uzblGlobal =<< ask
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

linkHover :: [String] -> UzblM ()
linkHover [u] = setVar "link_hovering" $ ValStr u
linkHover _ = badArgs

linkUnHover :: [String] -> UzblM ()
linkUnHover _ = setVar "link_hovering" $ ValStr ""

events :: Map.Map Event ([String] -> UzblM ())
events = Map.fromAscList $ map (first Event) $ 
  [ ("ADD_COOKIE",	addCookie)
  , ("COMMAND_ERROR",	commandError)
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
  , ("VARIABLE_SET",	variableSet) 
  ]

event :: Event -> [String] -> UzblM ()
event ev args = maybe nop
  ($ args) $ Map.lookup ev events
