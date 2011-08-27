module Bind 
  ( commandMode
  , rawMode
  , runBind
  ) where

import Prelude hiding (log)

import Control.Monad
import qualified Data.Map as Map
import Data.Maybe

import Util
import Config
import Uzbl
import Keys
import Cookies
import Prompt

pasteURI :: UzblM ()
pasteURI = io (capture "xclip" ["-o"]) >>= maybe nop goto

copyURI :: UzblM ()
copyURI = io . pipe "xclip" [] =<< uzblURI

rawBind :: ModKey -> UzblM ()
rawBind (0, "Escape") = do
  modifyBindings bindingsReturn
  setVar "forward_keys" $ ValInt 0
  resetVar "status_background"
rawBind _ = nop

rawMode :: UzblM ()
rawMode = do
  f <- getVar "forward_keys"
  when (f /= Just (ValInt 1)) $ do
    modifyBindings PassThrough
    setVar "forward_keys" $ ValInt 1
    status ""
    setVar "status_background" $ ValStr "#000"

search :: Bool -> String -> UzblM ()
search rev s = run $ "search" ++ (if rev then "_reverse" else "") ++ ' ' : escape s

cookieSave :: UzblM ()
cookieSave = do
  io . saveCookies (uzblHome "cookies.save") . uzblCookies =<< get
  status "cookies saved"

prompt :: String -> String -> (String -> UzblM ()) -> UzblM ()
prompt p i e = promptMode p i ((>>) commandMode . maybe nop e)

button2 :: UzblM ()
button2 = do
  l <- getVarStr "link_hovering"
  unless (null l) $ newUzbl $ Just l

scroll :: String -> String -> UzblM ()
scroll d s = runArgs "scroll" [d,s]

digit :: Int -> UzblM ()
digit i = do
  u@UzblState{ uzblBindings = b } <- get
  let c = i + maybe 0 (10*) (commandCount b)
  setVar "command_count" $ ValInt c
  put $ u{ uzblBindings = b{ commandCount = Just c } }

zero :: UzblM ()
zero = maybe 
  (scroll "vertical" "begin")
  (const $ digit 0) . commandCount . uzblBindings =<< get

countMaybe :: UzblM (Maybe Int)
countMaybe = do
  u@UzblState{ uzblBindings = b } <- get
  put $ u{ uzblBindings = b{ commandCount = Nothing } }
  setVar "command_count" $ ValStr ""
  return $ commandCount b

count :: UzblM Int
count = fromMaybe 1 =.< countMaybe

scaleCount :: Int -> UzblM String
scaleCount s = show . (s*) =.< count

scrlCount :: Bool -> UzblM String
scrlCount pos = scaleCount $ (if pos then id else negate) 20

toggleOrCount :: Variable -> [Value] -> UzblM ()
toggleOrCount v l = maybe t c =<< countMaybe where 
  t = toggleVar v l
  c i 
    | (ValInt _:_) <- l = setVarMsg v $ ValInt i
    | i <= length l = setVarMsg v (l !! pred i)
    | otherwise = t

commandBinds :: Map.Map ModKey (UzblM ())
commandBinds = Map.fromAscList $
  [ ((0, "$"),	        scroll "horizontal" "end")
  , ((0, "%"),	        toggleOrCount "disable_scripts" onOff)
  , ((0, "&"),	        toggleOrCount "stylesheet_uri" stylesheets)
  , ((0, "+"),	        run "zoom_in")
  , ((0, "/"),          prompt "/" "" $ search False)
  , ((0, "0"),	        zero)
  ] ++ 
  [ ((0, show i),       digit i) | i <- [1..9]
  ] ++
  [ ((0, ":"),	        prompt ":" "" $ \c -> run c)
  , ((0, "="),		setVar "zoom_level" (ValFloat 1))
  , ((0, "?"),          prompt "?" "" $ search True)
  , ((0, "@"),		toggleOrCount "caret_browsing" onOff)
  , ((0, "Button2"),	button2)
  , ((0, "Down"),	scroll "vertical" =<< scrlCount True)
  , ((0, "End"),	scroll "vertical" "end")
  , ((0, "Escape"),	commandMode)
  , ((0, "G"),	        scroll "vertical" "end")
  , ((0, "Home"),	scroll "vertical" "begin")
  , ((0, "K"),	        toggleOrCount "cookie_mode" $ map ValInt [0,1])
  , ((0, "L"),		run "search_reverse")
  , ((0, "Left"),       scroll "horizontal" =<< scrlCount False)
  , ((0, "O"),		uzblURI >>= \u -> prompt "uri " u goto)
  , ((0, "Page_Down"),	scroll "vertical" . (++"%") =<< scaleCount 100)
  , ((0, "Page_Up"),	scroll "vertical" . (++"%") =<< scaleCount (-100))
  , ((0, "Q"),	        run "exit")
  , ((0, "R"),		run "reload_ign_cache")
  , ((0, "Right"),	scroll "horizontal" =<< scrlCount True)
  , ((0, "Up"),		scroll "vertical" =<< scrlCount False)
  , ((0, "W"),		newUzbl . Just =<< uzblURI)
  , ((0, "\\"),		toggleOrCount "view_source" onOff >> run "reload")
  , ((0, "^"),	        scroll "horizontal" "begin")
  , ((0, "_"),	        run "zoom_out")
  , ((0, "e"),		runArgs "back" . return . show =<< count)
  , ((0, "h"),		scroll "horizontal" =<< scrlCount False)
  , ((0, "i"),		rawMode)
  , ((0, "l"),		run "search")
  , ((0, "n"),		scroll "vertical" =<< scrlCount False)
  , ((0, "o"),		prompt "uri " "" goto)
  , ((0, "p"),          pasteURI)
  , ((0, "r"),		run "reload")
  , ((0, "s"),		scroll "horizontal" =<< scrlCount True)
  , ((0, "space"),	scroll "vertical" . (++"%") =<< scaleCount 100)
  , ((0, "t"),		scroll "vertical" =<< scrlCount True)
  , ((0, "u"),		runArgs "forward" . return . show =<< count)
  , ((0, "v"),		run "toggle_status")
  , ((0, "y"),		copyURI)
  , ((0, "z"),		run "stop")
  , ((0, "{"),	        toggleOrCount "enable_spellcheck" onOff)
  , ((modCtrl, "k"),	cookieSave)
  , ((modMod1, "m"),    goto "~/.mozilla/bookmarks.html")
  , ((modMod1, "x"),    setVar "inject_html" $ ValStr $ "@(" ++ uzblHome "elinks-bookmarks" ++ ")@")
  ]

commandBind :: ModKey -> UzblM ()
commandBind = bindMap commandBinds (\_ -> log "no binding")

commandMode :: UzblM ()
commandMode = do
  status ""
  setVar "command_count" $ ValStr ""
  run "search_clear"
  modifyBindings $ const $ Command{ commandCount = Nothing }

runBind :: Bindings -> ModKey -> UzblM ()
runBind Command{} = commandBind
runBind PassThrough{} = rawBind
runBind Prompt{} = promptBind
