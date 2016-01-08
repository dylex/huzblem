{-# LANGUAGE PatternGuards, FlexibleContexts #-}
module Bind 
  ( commandMode
  , rawMode
  , runBind
  ) where

import Prelude hiding (log)

import Control.Concurrent.MVar
import Control.Monad
import Data.Char
import Data.List (isPrefixOf)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Time
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
search rev s = run $ "search " ++ (if rev then "rfind " else "find ") ++ escape s

cookieSave :: UzblM ()
cookieSave = do
  io . saveCookiesTxt (uzblHome "cookies.txt") =<< gets uzblCookies
  status "cookies saved"

promptComplete :: String -> String -> (String -> UzblM (Maybe String)) -> (String -> UzblM ()) -> UzblM ()
promptComplete p i c e = promptMode p i c ((>>) commandMode . maybe nop e)

prompt :: String -> String -> (String -> UzblM ()) -> UzblM ()
prompt p i = promptComplete p i (const $ return Nothing)

commandCompleter :: Completer
commandCompleter = f . breakStrip isSpace where
  f (c,"") = return $ completerSet commands c
  f ("set",vv) | (v,"") <- breakStrip isSpace vv = do
    vl <- gets uzblVariables
    return $ maybe
      ((\x -> "set " ++ x ++ " ") =.< completerSet (Map.keysSet vl) v)
      (\x -> Just $ "set " ++ v ++ " " ++ showValue x) $ Map.lookup v vl
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
  (const $ digit 0) =<< gets (commandCount . uzblBindings)

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

toggleOrCount :: Variable -> [Value] -> UzblM Value
toggleOrCount v l = onCount t c where 
  t = toggleVar v l
  c i 
    | (ValInt _:_) <- l = set (ValInt i)
    | i <= length l = set (l !! pred i)
    | otherwise = t
  set x = setVarMsg v x >. x

linkSelect :: String -> Maybe String -> UzblM ()
linkSelect n t = runScript $ scriptLinkSelect n t

promptURI :: (String -> UzblM ()) -> UzblM ()
promptURI = promptComplete "uri " "" (withDatabase . browseFind)

promptOpen :: UzblM ()
promptOpen = promptURI goto

toggleBlock :: String -> UzblM Value
toggleBlock t = toggleOrCount ("block_" ++ t) $ map (ValInt . fromEnum) [minBound..maxBound::BlockMode]

promptBlock :: Maybe Bool -> UzblM ()
promptBlock b = do
  u <- uzblURI
  prompt (maybe "unblock " (\t -> if t then "trust " else "block ") b) (fromMaybe "" $ uriDomain u) $ \d -> unless (null d) $ do
    bl <- asks (uzblBlocks . uzblGlobal)
    io $ modifyMVar_ bl (return . blockSet d b)
    updateScriptInit

promptMark :: Bool -> UzblM ()
promptMark f = do
  u <- uzblURI
  prompt (if f then "follow " else "mark ") u $
    withDatabase . (`markAdd` f)

listBrowse :: String -> [(String, Maybe String, Maybe LocalTime)] -> UzblM ()
listBrowse h b = runArgs "load" ["html",
    "<html><head><title>" ++ h ++ "</title></head><body><table>\
    \<col width='175px'/><col/><tbody>"
    ++ concatMap br b
    ++ "</tbody></table></html>", h]
  where 
  br (u, t, l) = "<tr>\
      \<td>" ++ maybe "" show l ++ "</td>\
      \<td><a href='" ++ u ++ "'>" ++ mlEscape (fromMaybe u t) ++ "</a></td>\
    \</tr>"

favorites :: Int -> UzblM ()
favorites n = listBrowse "Favorite history" . take n =<< withDatabase browseFavorites

marks :: UzblM ()
marks = listBrowse "Marks" =<< withDatabase markList

toggleStylesheet :: UzblM ()
toggleStylesheet = do
  css <- toggleOrCount "stylesheet_uri" stylesheets
  run "css clear"
  runArgs "css" ["add", showValue css, "all"]

improbableIslandKey :: ModKey -> UzblM ()
improbableIslandKey mk = do
  go mk =<< gets uzblLastLoad
  modifyBindings bindingsReturn
  resetVar "status_background"
  where
    go (0,[k]) (Just (p, l)) | isLower k || isDigit k = do
      modify $ \s -> s{ uzblLastLoad = Nothing }
      t <- io Data.Time.getCurrentTime
      let dt = Data.Time.diffUTCTime t l
      runScript $ scriptImprobableIslandKey p dt k
      status ("scheduled: " ++ [k])
    go _ _ = do
      status "aborted"
captureImprobableIslandKey :: UzblM ()
captureImprobableIslandKey = do
  u <- uzblURI
  when ("http://www.improbableisland.com/" `isPrefixOf` u) $ do
    status "schedule:"
    setVar "status_background" $ ValStr "#CC0"
    modifyBindings $ Capture improbableIslandKey

commandBinds :: Map.Map ModKey (UzblM ())
commandBinds = Map.fromAscList $
  [ ((0, "#"),          void $ toggleOrCount "link_number" onOff)
  , ((0, "$"),          scroll "horizontal" "end")
  , ((0, "%"),          void $ toggleOrCount "enable_scripts" onOff)
  , ((0, "&"),          void $ toggleStylesheet)
  , ((0, "*"),          void $ toggleOrCount "enable_webgl" onOff)
  , ((0, "+"),          run "zoom in")
  , ((0, "-"),		onCount
                          (newUzbl . Just =<< uzblURI)
                          (request "WINDOW" . scriptLinkGet . Just))
  , ((0, "/"),          prompt "/" "" $ search False)
  , ((0, "0"),	        zero)
  ] ++ 
  [ ((0, show i),       digit i) | i <- [1..9]
  ] ++
  [ ((0, ":"),          promptComplete ":" "" commandCompleter $ \c -> run c)
  , ((0, "?"),          prompt "?" "" $ search True)
  , ((0, "@"),	        void $ toggleOrCount "caret_browsing" onOff)
  , ((0, "="),		setVar "zoom_level" (ValFloat 1))
  , ((0, "A"),	        uzblURI >>= \u -> prompt "uri " u (newUzbl . Just))
  , ((0, "Button2"),	button2)
  , ((0, "Button8"),	run "back")
  , ((0, "Button9"),	pasteURI)
  , ((0, "Down"),	scroll "vertical" =<< scrlCount True)
  , ((0, "End"),	scroll "vertical" "end")
  , ((0, "Escape"),	commandMode)
  , ((0, "G"),	        scroll "vertical" "end")
  , ((0, "Home"),	scroll "vertical" "begin")
  , ((0, "ISO_Left_Tab"), runScript $ scriptKeydown (modShift,"U+0009"))
  , ((0, "J"),          prompt "js " "" $ run . ("js " ++))
  , ((0, "L"),	        run "search prev")
  , ((0, "Left"),       scroll "horizontal" =<< scrlCount False)
  , ((0, "M"),          goto "~/.mozilla/bookmarks.html")
  , ((0, "O"),	        uzblURI >>= \u -> prompt "uri " u goto)
  , ((0, "Page_Down"),	scroll "vertical" . (++"%") =<< scaleCount 100)
  , ((0, "Page_Up"),	scroll "vertical" . (++"%") =<< scaleCount (-100))
  , ((0, "Q"),	        run "exit")
  , ((0, "R"),	        run "reload full")
  , ((0, "Return"),	runScript . scriptActivate =<< countMaybe)
  , ((0, "Right"),	scroll "horizontal" =<< scrlCount True)
  , ((0, "Tab"),	runScript $ scriptKeydown (0,"U+0009"))
  , ((0, "Up"),		scroll "vertical" =<< scrlCount False)
  , ((0, "["),		linkSelect "prev" $ Just "\\bprev|^<")
  , ((0, "\\"),		toggleOrCount "view_source" onOff >> run "reload")
  , ((0, "]"),		linkSelect "next" $ Just "\\bnext|>$")
  , ((0, "^"),	        scroll "horizontal" "begin")
  , ((0, "_"),	        run "zoom out")
  , ((0, "`"),	        captureImprobableIslandKey)
  , ((0, "a"),		promptURI (newUzbl . Just))
  , ((0, "e"),		runArgs "back" . return . show =<< count)
  , ((0, "f"),		onCount 
                          (prompt "link " "" $ \t -> linkSelect t Nothing) 
                          (runScript . scriptFocus))
  , ((0, "h"),		scroll "horizontal" =<< scrlCount False)
  , ((0, "i"),		rawMode)
  , ((0, "l"),		run "search next")
  , ((0, "m"),          marks)
  , ((0, "n"),		scroll "vertical" =<< scrlCount False)
  , ((0, "o"),		promptOpen)
  , ((0, "p"),          pasteURI)
  , ((0, "r"),		run "reload")
  , ((0, "s"),		scroll "horizontal" =<< scrlCount True)
  , ((0, "space"),	scroll "vertical" . (++"%") =<< scaleCount 100)
  , ((0, "t"),		scroll "vertical" =<< scrlCount True)
  , ((0, "u"),		runArgs "forward" . return . show =<< count)
  , ((0, "v"),		void $ toggleOrCount "show_status" onOff)
  , ((0, "y"),		onCount copyURI (request "COPY" . scriptLinkGet . Just))
  , ((0, "z"),		run "stop")
  , ((0, "{"),	        void $ toggleOrCount "enable_spellcheck" onOff)
  , ((modCtrl, "m"),    promptMark True)
  , ((modMod1, "C"),    void $ toggleOrCount "cookie_policy" $ map ValInt [1,2,0])
  , ((modMod1, "a"),	void $ toggleOrCount "useragent" useragents) -- broken due to expansions...
  , ((modMod1, "b"),	promptBlock (Just False))
  , ((modMod1, "c"),	void $ toggleBlock "cookie")
  , ((modMod1, "f"),	void $ toggleBlock "iframe")
  , ((modMod1, "h"),	favorites . fromMaybe 50 =<< countMaybe)
  , ((modMod1, "i"),	void $ toggleBlock "img")
  , ((modMod1, "l"),    void $ toggleOrCount "enable_local_stoage" onOff)
  , ((modMod1, "m"),    promptMark False)
  , ((modMod1, "p"),    void $ toggleOrCount "enable_private" onOff)
  , ((modMod1, "s"),	void $ toggleBlock "script")
  , ((modMod1, "t"),	promptBlock (Just True))
  , ((modMod1, "u"),	promptBlock Nothing)
  , ((modMod1, "v"),	void $ toggleOrCount "block_verbose" onOff)
  , ((modMod1, "x"),    runArgs "load" ["html", "@(" ++ uzblHome "elinks-bookmarks" ++ ")@", "elinks-bookmarks"])
  , ((modCtrl .|. modMod1, "c"), cookieSave)
  ]

commandBind :: ModKey -> UzblM ()
commandBind = bindMap commandBinds (\_ -> debug "no binding")

commandMode :: UzblM ()
commandMode = do
  status ""
  setVar "command_count" $ ValStr ""
  run "search clear"
  modifyBindings $ const Command{ commandCount = Nothing }

runBind :: Bindings -> ModKey -> UzblM ()
runBind Command{} = commandBind
runBind PassThrough{} = rawBind
runBind Prompt{} = promptBind
runBind Capture{ captureFun = f } = f
