module Bind 
  ( readModifiers
  , defaultMode
  , rawMode
  ) where

import Prelude hiding (log)

import Control.Concurrent.MVar
import Control.Monad
import Data.Bits (bit, setBit, (.&.))
import Data.Char
import Data.List
import qualified Data.Map as Map

import Safe

import Util
import Config
import Uzbl
import Cookies

modifierList :: [String]
modifierList = [
    "Shift"
  , "ScrollLock"
  , "Ctrl"
  , "Mod1"
  , "Mod2"
  , "Mod3"
  , "Mod4"
  , "Mod5"
  , "Button1"
  , "Button2"
  , "Button3"
  , "Button4"
  , "Button5"
  ]

modifierIndex :: String -> Maybe Int
modifierIndex = (`elemIndex` modifierList)

modifier :: String -> Mod
modifier = maybe 0 bit . modifierIndex

modifiers :: [String] -> Mod
modifiers = foldl' (\i -> maybe i (setBit i) . modifierIndex) 0

readModifiers :: String -> Mod
readModifiers = modifiers . splitOn ('|'==) 

modCtrl, _modMod1, _modMod4 :: Mod
modCtrl = modifier "Ctrl"
_modMod1 = modifier "Mod1"
_modMod4 = modifier "Mod4"

modifierMask :: Mod
modifierMask = modifiers ["Ctrl","Mod1","Mod4"]

mask :: Mod -> ModKey -> ModKey
mask f (m,k) = (m.&.f,k)

bindMap :: Map.Map ModKey (UzblM ()) -> (ModKey -> UzblM ()) -> ModKey -> UzblM ()
bindMap m d k = maybe (d k) id $ Map.lookup (mask modifierMask k) m


pasteURI :: UzblM ()
pasteURI = io (capture "xclip" ["-o"]) >>= maybe nop uri

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

modifyPrompt :: (Prompt -> Prompt) -> UzblM ()
modifyPrompt f = modify $ \u -> u{ uzblPrompt = fmap f $ uzblPrompt u }

clearPrompt :: UzblM ()
clearPrompt = do
  modify $ \u -> u{ uzblPrompt = Nothing }
  defaultMode

promptRun :: UzblM ()
promptRun = do
  maybe nop (\Prompt{ promptExec = f, promptInput = (il, ir) } ->
    f $ reverse il ++ ir) . uzblPrompt =<< get
  clearPrompt

modifyPromptInput :: ((String,String) -> (String,String)) -> UzblM ()
modifyPromptInput f = modifyPrompt $ \p -> p{ promptInput = f $ promptInput p }

promptInsert :: String -> UzblM ()
promptInsert s = modifyPromptInput $ first (reverse s++)

inputLeft :: (String,String) -> (String,String)
inputLeft (c:il,ir) = (il,c:ir)
inputLeft i = i

inputRight :: (String,String) -> (String,String)
inputRight (il,c:ir) = (c:il,ir)
inputRight i = i

promptBinds :: Map.Map ModKey (UzblM ())
promptBinds = Map.fromAscList 
  [ ((0, "BackSpace"),  modifyPromptInput $ first tailSafe)
  , ((0, "Delete"),     modifyPromptInput $ second tailSafe)
  , ((0, "End"),        modifyPromptInput $ \(il,ir) -> (reverse ir++il,""))
  , ((0, "Escape"),     clearPrompt)
  , ((0, "Home"),       modifyPromptInput $ \(il,ir) -> ("",reverse il++ir))
  , ((0, "Left"),       modifyPromptInput inputLeft)
  , ((0, "Return"),     promptRun)
  , ((0, "Right"),      modifyPromptInput inputRight)
  , ((0, "space"),      promptInsert " ")
  , ((modCtrl, "u"),    modifyPromptInput $ const ("",""))
  , ((modCtrl, "w"),    modifyPromptInput $ \(il,ir) -> (tailSafe $ dropWhile (not . isSpace) il,ir))
  ]

setPrompt :: String -> UzblM ()
setPrompt = setVar "status_prompt" . ValStr

promptUpdate :: UzblM ()
promptUpdate =
  maybe (setPrompt "") (\Prompt{ promptPrompt = p, promptInput = (il,ir) } ->
    setPrompt $ "<span bgcolor='#000'>" ++ p ++ "<span face='monospace'>" 
      ++ mlEscape (reverse il) ++ "<span face='sans'>|</span>" ++ mlEscape ir 
      ++ "</span></span>"
    ) . uzblPrompt =<< get

promptBind :: ModKey -> UzblM ()
promptBind mk = do
  bindMap promptBinds (promptInsert . snd) mk
  promptUpdate

promptMode :: Prompt -> UzblM ()
promptMode p = do
  modify $ \u -> u
    { uzblBind = promptBind
    , uzblPrompt = Just p
    }   
  promptUpdate

prompt :: String -> String -> (String -> UzblM ()) -> UzblM ()
prompt p i e = promptMode $ Prompt
  { promptPrompt = p
  , promptInput = (reverse i,"")
  , promptExec = e
  }

search :: Bool -> String -> UzblM ()
search rev s = run ("search" ++ (if rev then "_reverse" else "") ++ ' ' : escape s) []

cookieSave :: UzblM ()
cookieSave = ask >>= io . 
  (saveCookies (uzblHome "cookies.save") <=< readMVar . uzblCookies . uzblGlobal)

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
  , ((0, "Down"),	run "scroll" ["vertical", scrl])
  , ((0, "End"),	run "scroll" ["vertical", "end"])
  , ((0, "G"),	        run "scroll" ["vertical", "end"])
  , ((0, "Home"),	run "scroll" ["vertical", "begin"])
  , ((0, "K"),	        toggleVar "cookie_mode" $ map ValInt [0,1])
  , ((0, "L"),		run "search_reverse" [])
  , ((0, "Left"),       run "scroll" ["horizontal", '-':scrl])
  , ((0, "O"),		uzblURI >>= \u -> prompt "uri " u uri)
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
  , ((0, "o"),		prompt "uri " "" uri)
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
defaultMode = modify $ \u -> u{ uzblBind = defaultBind }
