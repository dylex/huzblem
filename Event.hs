module Event
  ( event
  ) where

import Prelude hiding (log)

import qualified Data.Map as Map

import Util
import Config
import Uzbl
import Bind
import Cookies

badArgs :: UzblM ()
badArgs = log "unknown arguments"

commandError :: [String] -> UzblM ()
commandError [_] = log ""
commandError _ = badArgs

variableSet :: [String] -> UzblM ()
variableSet [var,typ,sval] | Just val <- readValue typ sval =
  modify $ \u -> u { uzblVariables = Map.insert var val (uzblVariables u) }
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
newWindow [u] = uri u
newWindow _ = badArgs

addCookie :: [String] -> UzblM ()
addCookie = maybe badArgs ac . argCookie where
  ac c = io $ print c

events :: Map.Map Event EventHandler
events = Map.fromAscList $ map (first Event) $ 
  [ ("ADD_COOKIE",    addCookie)
  , ("COMMAND_ERROR", commandError)
  , ("FIFO_SET",      fifoSet)
  , ("FORM_ACTIVE",   \_ -> rawMode)
  , ("KEY_PRESS",     keyPress)
  , ("NEW_WINDOW",    newWindow)
  , ("SOCKET_SET",    socketSet) 
  , ("VARIABLE_SET",  variableSet) 
  ]

event :: Event -> [String] -> UzblM ()
event ev args = maybe nop
  ($ args) $ Map.lookup ev events
