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
  | ValFloat Float
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
  , "Mozilla/5.0 (X11; " ++ uname ++ ") AppleWebKit/555 (KHTML, like Gecko) Chrome/25 Safari/555"
  , "Mozilla/5.0 (X11; " ++ uname ++ "; rv:5.5) Gecko/" ++ today ++ " Firefox/5.5"
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
  , ("download_handler",        ValStr $ "sync_spawn " ++ uzblHome "download" ++ " \\@download_dir")
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
  , ("zoom_type",		ValInt 0)
  ] `Map.union` baseConfig

runUzbl :: FilePath -> Cookies -> Config -> Maybe String -> IO ()
runUzbl sock cookies config uri = do
  let args = ["--connect-socket", sock, "--config", "-"] ++ maybe [] (("-u":) . return) uri
  (Just h, _, _, pid) <- createProcess (proc "uzbl-core" args){ std_in = CreatePipe }
  mapM_ (\(k,v) -> hPutStrLn h $ "set " ++ k ++ '=' : showValue v) $ Map.toList $ Map.union baseConfig $ Map.delete "uri" config
  mapM_ (\a -> hPutStrLn h $ unwords $ "add_cookie" : map quote a) $ cookiesArgs cookies
  hClose h
  void $ forkIO $ void $ waitForProcess pid

commands :: Set.Set String
commands = Set.fromAscList
  [ "add_cookie"
  , "back"
  , "chain"
  , "clear_cookies"
  , "dehilight"
  , "delete_cookie"
  , "download"
  , "dump_config"
  , "dump_config_as_events"
  , "event"
  , "exit"
  , "forward"
  , "hardcopy"
  , "include"
  , "js"
  , "menu_add"
  , "menu_editable_add"
  , "menu_editable_remove"
  , "menu_editable_separator"
  , "menu_image_add"
  , "menu_image_remove"
  , "menu_image_separator"
  , "menu_link_add"
  , "menu_link_remove"
  , "menu_link_separator"
  , "menu_remove"
  , "menu_separator"
  , "print"
  , "reload"
  , "reload_ign_cache"
  , "request"
  , "script"
  , "scroll"
  , "search"
  , "search_clear"
  , "search_reverse"
  , "set"
  , "sh"
  , "show_inspector"
  , "spawn"
  , "stop"
  , "sync_sh"
  , "sync_spawn"
  , "sync_spawn_exec"
  , "toggle"
  , "uri"
  , "zoom_in"
  , "zoom_out"
  ]
