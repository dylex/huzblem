module Config
  ( Variable
  , Value(..), readValue, showValue
  , Config

  , home, uzblHome
  , defaultConfig
  , stylesheets
  , useragents
  , runUzbl
  , commands
  ) where

import Control.Concurrent
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Time as Time
import System.Environment
import System.FilePath
import System.IO
import qualified System.IO.Unsafe as Unsafe
import System.Locale (defaultTimeLocale)
import System.Process

import Safe

import Util
import Cookies
import Block

type Variable = String

data Value 
  = ValNone
  | ValInt Int
  | ValFloat Double
  | ValStr String

type Config = Map.Map Variable Value

instance Eq Value where
  ValNone    == ValNone    = True
  ValInt   x == ValInt   y = x == y
  ValInt   x == ValFloat y = fromIntegral x == y
  ValInt   x == ValStr   y = Just x == readMay y
  ValFloat x == ValInt   y = x == fromIntegral y
  ValFloat x == ValFloat y = x == y
  ValFloat x == ValStr   y = Just x == readMay y
  ValStr   x == ValInt   y = readMay x == Just y
  ValStr   x == ValFloat y = readMay x == Just y
  ValStr   x == ValStr   y = x == y
  _          == _          = False

readValue :: String -> String -> Maybe Value
readValue "none" _ = Just ValNone
readValue "int" s = fmap ValInt $ readMay s
readValue "float" s = fmap ValFloat $ readMay s
readValue "str" s = Just $ ValStr s
readValue "" "" = Just ValNone
readValue "" s = Just $ maybe (maybe (ValStr s) ValFloat (readMay s)) ValInt (readMay s)
readValue _ _ = Nothing

showValue :: Value -> String
showValue ValNone = ""
showValue (ValInt i) = show i
showValue (ValFloat f) = show f
showValue (ValStr s) = s

instance Show Value where
  showsPrec _ ValNone = id
  showsPrec p (ValInt x) = showsPrec p x
  showsPrec p (ValFloat x) = showsPrec p x
  showsPrec p (ValStr x) = showsPrec p x

home :: FilePath
home = Unsafe.unsafeDupablePerformIO $ getEnv "HOME"

uzblHome :: FilePath -> FilePath
uzblHome = ((home </> ".uzbl") </>)

stylesheets :: [Value]
stylesheets = map (ValStr . ("file://" ++) . uzblHome) ["plain.css", "style.css"]

useragents :: [Value]
useragents = map ValStr 
  [ "Uzbl (X11; " ++ uname ++ ") WebKit/@{WEBKIT_MAJOR}.@{WEBKIT_MINOR}"
  , "Mozilla/5.0 (X11; " ++ uname ++ ") AppleWebKit/537 (KHTML, like Gecko) Chrome/25 Safari/537"
  , "Mozilla/5.0 (X11; " ++ uname ++ "; rv:21.0) Gecko/" ++ today ++ " Firefox/21.0"
  ]
  where 
    uname = maybe "unknown" (head . lines) $ Unsafe.unsafeDupablePerformIO $ capture "uname" ["-sm"]
    today = Time.formatTime defaultTimeLocale "%Y%m%d" $ Time.utctDay $ Unsafe.unsafeDupablePerformIO Time.getCurrentTime

-- |These variables are reset on start.  Any setting containing an expansion must be here.
baseConfig :: Config
baseConfig = Map.fromAscList
  [ ("fifo_dir",		ValStr "/tmp")
  , ("forward_keys",		ValInt 0)
  , ("socket_dir",		ValStr "/tmp")
  , ("status_background",	ValStr "#AAA")
  , ("status_format",		ValStr "<span bgcolor='#8CC' color='#000'>\\@command_count</span>\\@status_message<span color='#440'>\\@[\\@SELECTED_URI]\\@</span>")
  , ("status_format_right",	ValStr "<span color='#FFF' bgcolor='\\@status_load_color'>\\@status_load</span> <span color='#08F'>\\@[\\@uri]\\@</span>")
  , ("status_load",		ValStr "")
  , ("status_load_color",	ValStr "#000")
  , ("title_format_long",	ValStr "\\@TITLE - Uzbl <\\@NAME> \\@SELECTED_URI")
  , ("title_format_short",	ValStr "uzbl \\@TITLE")
  , ("uzbl_home",		ValStr $ uzblHome "")
  ]

-- |These variables can be overridden and inherited by new windows.
defaultConfig :: Config
defaultConfig = Map.fromAscList 
  [ ("autoload_images",         ValInt 1)
  , ("block_cookie",		ValInt $ fromEnum AllowTrusted)
  , ("block_embed",		ValInt $ fromEnum BlockAll)
  , ("block_iframe",		ValInt $ fromEnum AllowTrusted{-Current-})
  , ("block_img",		ValInt $ fromEnum BlockUntrusted)
  , ("block_script",		ValInt $ fromEnum AllowTrusted)
  , ("block_verbose",		ValInt 1)
  , ("caret_browsing",		ValInt 1)
  , ("cookie_policy",           ValInt 2)
  , ("developer_extras",	ValInt 1)
  , ("dns_prefetching",	        ValInt 0)
  , ("enable_plugins",		ValInt 0)
  , ("enable_scripts",		ValInt 1)
  , ("download_dir",            ValStr $ home </> "tmp/uzbl")
  , ("download_handler",        ValStr $ "spawn_sync " ++ uzblHome "download" ++ " \\@download_dir")
  , ("enable_local_storage",	ValInt 0)
  , ("enable_offline_app_cache",ValInt 0)
  , ("enable_page_cache",	ValInt 1)
  , ("enable_private",	        ValInt 0)
  , ("enable_spellcheck",	ValInt 0)
  , ("enable_webgl",	        ValInt 0)
  , ("javascript_windows",      ValInt 0)
  , ("link_number",             ValInt 0)
  , ("shell_cmd",		ValStr $ uzblHome "shell")
  , ("show_status",		ValInt 1)
  , ("status_top",		ValInt 0)
  , ("stylesheet_uri",		head stylesheets)
  , ("useragent",		head useragents)
  , ("zoom_text_only",		ValInt 1)
  ] `Map.union` baseConfig

runUzbl :: FilePath -> Cookies -> Config -> Bool -> Maybe String -> IO ()
runUzbl sock cookies config debug uri = do
  let args = ["--connect-socket", sock, "--config", "-"] ++ (guard debug >. "-v") ++ maybe [] (("-u":) . return) uri
  (Just h, _, _, pid) <- createProcess (proc "uzbl-core" args){ std_in = CreatePipe }
  mapM_ (\(k,v) -> hPutStrLn h $ "set " ++ k ++ ' ' : showValue v) $ Map.toList $ Map.union baseConfig $ Map.delete "uri" config
  mapM_ (\a -> hPutStrLn h $ unwords $ "cookie add" : map quote a) $ cookiesArgs cookies
  hPutStrLn h "cookie store cookies2.txt"
  hPutStrLn h ("css add " ++ quote (showValue (head stylesheets)) ++ " all")
  hClose h
  void $ forkIO $ void $ waitForProcess pid

commands :: Set.Set String
commands = Set.fromAscList
  [ "back"
  , "cache"
  , "chain"
  , "cookie"
  , "css"
  , "dns"
  , "download"
  , "dump_config"
  , "dump_config_as_events"
  , "event"
  , "exit"
  , "favicon"
  , "forward"
  , "geometry"
  , "hardcopy"
  , "include"
  , "inspector"
  , "js"
  , "load"
  , "menu"
  , "print"
  , "reload"
  , "remove_all_db"
  , "request"
  , "save"
  , "scheme"
  , "scroll"
  , "search"
  , "security"
  , "set"
  , "snapshot"
  , "spawn"
  , "spawn_sh"
  , "spawn_sh_sync"
  , "spawn_sync"
  , "spawn_sync_exec"
  , "spell"
  , "stop"
  , "toggle"
  , "uri"
  , "zoom"
  ]
