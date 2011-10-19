module Bind 
  ( commandMode
  , rawMode
  , runBind
  ) where

import Prelude hiding (log)

import Control.Concurrent.MVar
import Control.Monad
import Data.Char
import qualified Data.Map as Map
import Data.Maybe
import Data.Time.LocalTime (LocalTime)

import Util
import Config
import Uzbl
import Keys
import Cookies
import Database
import Prompt
import Scripts
import URIs
import Block

pasteURI :: UzblM ()
pasteURI = io paste >>= maybe nop goto

copyURI :: UzblM ()
copyURI = io . copy =<< uzblURI

rawBind :: ModKey -> UzblM ()
rawBind (0, "Escape") = do
  modifyBindings bindingsReturn
  setVar "forward_keys" $ ValInt 0
  resetVar "status_background"
rawBind _ = nop

rawMode :: UzblM ()
rawMode = do
  f <- getVar "forward_keys"
  when (f /= ValInt 1) $ do
    modifyBindings PassThrough
    setVar "forward_keys" $ ValInt 1
    status ""
    setVar "status_background" $ ValStr "#000"

search :: Bool -> String -> UzblM ()
search rev s = run $ "search" ++ (guard rev >> "_reverse") ++ ' ' : escape s

cookieSave :: UzblM ()
cookieSave = do
  io . saveCookiesTxt (uzblHome "cookies.txt") . uzblCookies =<< get
  status "cookies saved"

promptComplete :: String -> String -> (String -> UzblM (Maybe String)) -> (String -> UzblM ()) -> UzblM ()
promptComplete p i c e = promptMode p i c ((>>) commandMode . maybe nop e)

prompt :: String -> String -> (String -> UzblM ()) -> UzblM ()
prompt p i e = promptComplete p i (const $ return Nothing) e

commandCompleter :: Completer
commandCompleter = f . breakStrip isSpace where
  f (c,"") = return $ completerSet commands c
  f ("set",vv) | (v,"") <- breakStrip (\x -> '=' == x || isSpace x) vv = do
    vl <- uzblVariables =.< get
    return $ maybe
      ((\x -> "set " ++ x ++ "=") =.< completerSet (Map.keysSet vl) v)
      (\x -> Just $ "set " ++ v ++ "=" ++ showValue x) $ Map.lookup v vl
  f _ = return Nothing

button2 :: UzblM ()
button2 = do
  l <- getVarStr "SELECTED_URI"
  if null l
    then nop
    else if takeWhile (/= '.') (reverse l) `elem` map reverse ["bz2","flac","gif","gz","jpeg","jpg","mp3","pdf","png","xz","zip"]
      then runArgs "download" [l]
      else newUzbl $ Just l

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

onCount :: UzblM a -> (Int -> UzblM a) -> UzblM a
onCount n y = maybe n y =<< countMaybe

count :: UzblM Int
count = fromMaybe 1 =.< countMaybe

scaleCount :: Int -> UzblM String
scaleCount s = show . (s*) =.< count

scrlCount :: Bool -> UzblM String
scrlCount pos = scaleCount $ (if pos then id else negate) 20

toggleOrCount :: Variable -> [Value] -> UzblM ()
toggleOrCount v l = onCount t c where 
  t = toggleVar v l
  c i 
    | (ValInt _:_) <- l = setVarMsg v (ValInt i)
    | i <= length l = setVarMsg v (l !! pred i)
    | otherwise = t

linkSelect :: String -> Maybe String -> UzblM ()
linkSelect n t = runScript $ scriptLinkSelect n t

promptURI :: (String -> UzblM ()) -> UzblM ()
promptURI = promptComplete "uri " "" (withDatabase . browseFind)

promptOpen :: UzblM ()
promptOpen = promptURI goto

toggleBlock :: String -> UzblM ()
toggleBlock t = toggleOrCount ("block_" ++ t) $ map (ValInt . fromEnum) [minBound..maxBound::BlockMode]

promptBlock :: Maybe Bool -> UzblM ()
promptBlock b = do
  u <- uzblURI
  prompt (maybe "unblock " (\t -> if t then "trust " else "block ") b) (fromMaybe "" $ uriDomain u) $ \d -> do
    bl <- uzblBlocks . uzblGlobal =.< ask
    io $ modifyMVar_ bl (return . blockSet d b)
    updateScriptInit

promptMark :: Bool -> UzblM ()
promptMark f = do
  u <- uzblURI
  prompt (if f then "follow " else "mark ") u $
    withDatabase . (`markAdd` f)

listBrowse :: String -> [(String, Maybe String, Maybe LocalTime)] -> UzblM ()
listBrowse h b = setVar "inject_html" $ ValStr $
    "<html><head><title>" ++ h ++ "</title></head><body><table>\
    \<col width='175px'/><col/><tbody>"
    ++ concatMap br b
    ++ "</tbody></table></html>"
  where 
  br (u, t, l) = "<tr>\
      \<td>" ++ maybe "" show l ++ "</td>\
      \<td><a href='" ++ u ++ "'>" ++ mlEscape (fromMaybe u t) ++ "</a></td>\
    \</tr>"

favorites :: Int -> UzblM ()
favorites n = listBrowse "Favorite history" . take n =<< withDatabase browseFavorites

marks :: UzblM ()
marks = listBrowse "Marks" =<< withDatabase markList

commandBinds :: Map.Map ModKey (UzblM ())
commandBinds = Map.fromAscList $
  [ ((0, "-"),		onCount
                          (newUzbl . Just =<< uzblURI)
                          (request "WINDOW" . scriptLinkGet . Just))
  , ((0, "/"),          prompt "/" "" $ search False)
  , ((0, "0"),	        zero)
  ] ++ 
  [ ((0, show i),       digit i) | i <- [1..9]
  ] ++
  [ ((0, "="),		setVar "zoom_level" (ValFloat 1))
  , ((0, "Button2"),	button2)
  , ((0, "Down"),	scroll "vertical" =<< scrlCount True)
  , ((0, "End"),	scroll "vertical" "end")
  , ((0, "Escape"),	commandMode)
  , ((0, "Home"),	scroll "vertical" "begin")
  , ((0, "Left"),       scroll "horizontal" =<< scrlCount False)
  , ((0, "Page_Down"),	scroll "vertical" . (++"%") =<< scaleCount 100)
  , ((0, "Page_Up"),	scroll "vertical" . (++"%") =<< scaleCount (-100))
  , ((0, "Return"),	runScript . scriptActivate =<< countMaybe)
  , ((0, "Right"),	scroll "horizontal" =<< scrlCount True)
  , ((0, "Tab"),	runScript $ scriptKeydown (0,"U+0009"))
  , ((0, "Up"),		scroll "vertical" =<< scrlCount False)
  , ((0, "["),		linkSelect "prev" $ Just "\\\\bprev|^<")
  , ((0, "\\"),		toggleOrCount "view_source" onOff >> run "reload")
  , ((0, "]"),		linkSelect "next" $ Just "\\\\bnext|>$")
  , ((0, "a"),		promptURI (newUzbl . Just))
  , ((0, "e"),		runArgs "back" . return . show =<< count)
  , ((0, "f"),		onCount 
                          (prompt "link " "" $ \t -> linkSelect t Nothing) 
                          (runScript . scriptFocus))
  , ((0, "h"),		scroll "horizontal" =<< scrlCount False)
  , ((0, "i"),		rawMode)
  , ((0, "l"),		run "search")
  , ((0, "m"),          promptMark False)
  , ((0, "n"),		scroll "vertical" =<< scrlCount False)
  , ((0, "o"),		promptOpen)
  , ((0, "p"),          pasteURI)
  , ((0, "r"),		run "reload")
  , ((0, "s"),		scroll "horizontal" =<< scrlCount True)
  , ((0, "space"),	scroll "vertical" . (++"%") =<< scaleCount 100)
  , ((0, "t"),		scroll "vertical" =<< scrlCount True)
  , ((0, "u"),		runArgs "forward" . return . show =<< count)
  , ((0, "v"),		toggleOrCount "show_status" onOff)
  , ((0, "y"),		onCount copyURI (request "COPY" . scriptLinkGet . Just))
  , ((0, "z"),		run "stop")
  , ((modShift, "#"),   toggleOrCount "link_number" onOff)
  , ((modShift, "$"),   scroll "horizontal" "end")
  , ((modShift, "%"),   toggleOrCount "enable_scripts" onOff)
  , ((modShift, "&"),   toggleOrCount "stylesheet_uri" stylesheets)
  , ((modShift, "+"),   run "zoom_in")
  , ((modShift, ":"),   promptComplete ":" "" commandCompleter $ \c -> run c)
  , ((modShift, "?"),   prompt "?" "" $ search True)
  , ((modShift, "@"),	toggleOrCount "caret_browsing" onOff)
  , ((modShift, "A"),	uzblURI >>= \u -> prompt "uri " u (newUzbl . Just))
  , ((modShift, "G"),	scroll "vertical" "end")
  , ((modShift, "ISO_Left_Tab"),  runScript $ scriptKeydown (modShift,"U+0009"))
  , ((modShift, "L"),	run "search_reverse")
  , ((modShift, "M"),   promptMark True)
  , ((modShift, "O"),	uzblURI >>= \u -> prompt "uri " u goto)
  , ((modShift, "Q"),	run "exit")
  , ((modShift, "R"),	run "reload_ign_cache")
  , ((modShift, "^"),	scroll "horizontal" "begin")
  , ((modShift, "_"),	run "zoom_out")
  , ((modShift, "{"),	toggleOrCount "enable_spellcheck" onOff)
  , ((modMod1, "a"),	toggleOrCount "useragent" useragents) -- broken due to expansions...
  , ((modMod1, "b"),	promptBlock (Just False))
  , ((modMod1, "c"),	toggleBlock "cookie")
  , ((modMod1, "f"),	toggleBlock "iframe")
  , ((modMod1, "h"),	favorites . fromMaybe 50 =<< countMaybe)
  , ((modMod1, "i"),	toggleBlock "img")
  , ((modMod1, "m"),    marks)
  , ((modMod1, "p"),    toggleOrCount "enable_private" onOff)
  , ((modMod1, "s"),	toggleBlock "script")
  , ((modMod1, "t"),	promptBlock (Just True))
  , ((modMod1, "u"),	promptBlock Nothing)
  , ((modMod1, "v"),	toggleOrCount "block_verbose" onOff)
  , ((modMod1, "x"),    setVar "inject_html" $ ValStr $ "@(" ++ uzblHome "elinks-bookmarks" ++ ")@")
  , ((modShift .|. modMod1, "C"), cookieSave)
  , ((modShift .|. modMod1, "M"), goto "~/.mozilla/bookmarks.html")
  ]

commandBind :: ModKey -> UzblM ()
commandBind = bindMap commandBinds (\_ -> debug "no binding")

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
