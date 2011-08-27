module Bind 
  ( defaultMode
  , rawMode
  ) where

import Prelude hiding (log)

import Control.Monad
import qualified Data.Map as Map

import Util
import Config
import Uzbl
import Keys
import Cookies
import qualified Prompt

pasteURI :: UzblM ()
pasteURI = io (capture "xclip" ["-o"]) >>= maybe nop goto

copyURI :: UzblM ()
copyURI = io . pipe "xclip" [] =<< uzblURI

scrl :: String
scrl = "20"

rawBind :: (ModKey -> UzblM ()) -> ModKey -> UzblM ()
rawBind defbind (0, "Escape") = do
  modify $ \u -> u{ uzblBind = defbind }
  setVar "forward_keys" $ ValInt 0
  resetVar "status_background"
rawBind _ _ = nop

rawMode :: UzblM ()
rawMode = do
  f <- getVar "forward_keys"
  when (f /= Just (ValInt 1)) $ do
    modify $ \u -> u{ uzblBind = rawBind (uzblBind u) }   
    setVar "forward_keys" $ ValInt 1
    setVar "status_background" $ ValStr "#000"

search :: Bool -> String -> UzblM ()
search rev s = run ("search" ++ (if rev then "_reverse" else "") ++ ' ' : escape s) []

cookieSave :: UzblM ()
cookieSave = io . saveCookies (uzblHome "cookies.save") . uzblCookies =<< get

prompt :: String -> String -> (String -> UzblM ()) -> UzblM ()
prompt p i e = Prompt.prompt p i ((>>) defaultMode . maybe nop e)

button2 :: UzblM ()
button2 = do
  l <- getVarStr "link_hovering"
  unless (null l) $ newUzbl $ Just l

defaultBinds :: Map.Map ModKey (UzblM ())
defaultBinds = Map.fromAscList 
  [ ((0, "$"),	        run "scroll" ["horizontal", "end"])
  , ((0, "%"),	        toggleVar "disable_scripts" onOff)
  , ((0, "&"),	        toggleVar "stylesheet_uri" stylesheets)
  , ((0, "+"),	        run "zoom_in" [])
  , ((0, "/"),          prompt "/" "" $ search False)
  , ((0, "0"),	        run "scroll" ["vertical", "begin"])
  , ((0, ":"),	        prompt ":" "" $ \c -> run c [])
  , ((0, "="),		setVar "zoom_level" (ValFloat 1))
  , ((0, "?"),          prompt "?" "" $ search True)
  , ((0, "@"),		toggleVar "caret_browsing" onOff)
  , ((0, "B"),	        setVar "inject_html" $ ValStr $ "@(" ++ uzblHome "elinks-bookmarks" ++ ")@")
  , ((0, "Button2"),	button2)
  , ((0, "Down"),	run "scroll" ["vertical", scrl])
  , ((0, "End"),	run "scroll" ["vertical", "end"])
  , ((0, "Escape"),	defaultMode)
  , ((0, "G"),	        run "scroll" ["vertical", "end"])
  , ((0, "Home"),	run "scroll" ["vertical", "begin"])
  , ((0, "K"),	        toggleVar "cookie_mode" $ map ValInt [0,1])
  , ((0, "L"),		run "search_reverse" [])
  , ((0, "Left"),       run "scroll" ["horizontal", '-':scrl])
  , ((0, "O"),		uzblURI >>= \u -> prompt "uri " u goto)
  , ((0, "Page_Down"),	run "scroll" ["vertical", "100%"])
  , ((0, "Page_Up"),	run "scroll" ["vertical", "-100%"])
  , ((0, "Q"),	        run "exit" [])
  , ((0, "R"),		run "reload_ign_cache" [])
  , ((0, "Right"),	run "scroll" ["horizontal", scrl])
  , ((0, "Up"),		run "scroll" ["vertical", '-':scrl])
  , ((0, "W"),		newUzbl . Just =<< uzblURI)
  , ((0, "\\"),		toggleVar "view_source" onOff >> run "reload" [])
  , ((0, "^"),	        run "scroll" ["horizontal", "begin"])
  , ((0, "_"),	        run "zoom_out" [])
  , ((0, "e"),		run "back" [])
  , ((0, "h"),		run "scroll" ["horizontal", '-':scrl])
  , ((0, "i"),		rawMode)
  , ((0, "l"),		run "search" [])
  , ((0, "n"),		run "scroll" ["vertical", '-':scrl])
  , ((0, "o"),		prompt "uri " "" goto)
  , ((0, "p"),          pasteURI)
  , ((0, "r"),		run "reload" [])
  , ((0, "s"),		run "scroll" ["horizontal", scrl])
  , ((0, "space"),	run "scroll" ["vertical", "100%"])
  , ((0, "t"),		run "scroll" ["vertical", scrl])
  , ((0, "u"),		run "forward" [])
  , ((0, "v"),		run "toggle_status" [])
  , ((0, "y"),		copyURI)
  , ((0, "z"),		run "stop" [])
  , ((0, "{"),	        toggleVar "enable_spellcheck" onOff)
  , ((modCtrl, "k"),	cookieSave)
  ]

defaultBind :: ModKey -> UzblM ()
defaultBind = bindMap defaultBinds (\_ -> log "no binding")

defaultMode :: UzblM ()
defaultMode = do
  status ""
  modify $ \u -> u{ uzblBind = defaultBind }
