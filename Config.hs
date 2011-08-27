module Config
  ( Variable
  , Value(..), readValue, showValue
  , Config

  , home, uzblHome
  , defaultConfig
  , stylesheets
  , runUzbl
  ) where

import Control.Concurrent
import Control.Monad
import qualified Data.Map as Map
import System.Environment
import System.FilePath
import System.IO
import qualified System.IO.Unsafe as Unsafe
import System.Process

import Safe

import Util
import Cookies

type Variable = String

data Value 
  = ValInt Int
  | ValFloat Float
  | ValStr String

type Config = Map.Map Variable Value

instance Eq Value where
  ValInt   x == ValInt   y = x == y
  ValInt   x == ValFloat y = fromIntegral x == y
  ValInt   x == ValStr   y = Just x == readMay y
  ValFloat x == ValInt   y = x == fromIntegral y
  ValFloat x == ValFloat y = x == y
  ValFloat x == ValStr   y = Just x == readMay y
  ValStr   x == ValInt   y = readMay x == Just y
  ValStr   x == ValFloat y = readMay x == Just y
  ValStr   x == ValStr   y = x == y

readValue :: String -> String -> Maybe Value
readValue "int" s = fmap ValInt $ readMay s
readValue "float" s = fmap ValFloat $ readMay s
readValue "str" s = Just $ ValStr s
readValue _ _ = Nothing

showValue :: Value -> String
showValue (ValInt i) = show i
showValue (ValFloat f) = show f
showValue (ValStr s) = s

instance Show Value where
  showsPrec p (ValInt x) = showsPrec p x
  showsPrec p (ValFloat x) = showsPrec p x
  showsPrec p (ValStr x) = showsPrec p x


home :: FilePath
home = Unsafe.unsafeDupablePerformIO $ getEnv "HOME"

uzblHome :: FilePath -> FilePath
uzblHome = ((home </> ".uzbl") </>)

stylesheets :: [Value]
stylesheets = map (ValStr . ("file://" ++) . uzblHome) ["plain.css", "style.css"]

-- |These variables are reset on start.  Any setting containing an expansion must be here.
baseConfig :: Config
baseConfig = Map.fromAscList
  [ ("fifo_dir",		ValStr $ uzblHome "")
  , ("forward_keys",		ValInt 0)
  , ("socket_dir",		ValStr $ uzblHome "")
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
defaultConfig = Map.union (Map.fromAscList 
  [ ("caret_browsing",		ValInt 1)
  , ("cookie_mode",		ValInt 0)
  , ("disable_plugins",		ValInt 1)
  , ("disable_scripts",		ValInt 0)
  , ("enable_spellcheck",	ValInt 0)
  , ("shell_cmd",		ValStr $ uzblHome "shell")
  , ("show_status",		ValInt 1)
  , ("status_top",		ValInt 0)
  , ("stylesheet_uri",		head stylesheets)
  , ("useragent",		ValStr $ uzblHome "Uzbl (Webkit @{WEBKIT_MAJOR}.@{WEBKIT_MINOR}) (@(+uname -sm)@ [@ARCH_UZBL])")
  , ("zoom_type",		ValInt 0)
  ]) baseConfig

runUzbl :: FilePath -> Cookies -> Config -> Maybe String -> IO ()
runUzbl sock cookies config uri = do
  let args = ["--connect-socket", sock, "--config", "-"] ++ maybe [] (("-u":) . return) uri
  print args
  (Just h, _, _, pid) <- createProcess (proc "uzbl-core" args){ std_in = CreatePipe }
  mapM_ (\(k,v) -> hPutStrLn h $ "set " ++ k ++ '=' : showValue v) $ Map.toList $ Map.union baseConfig config
  mapM_ (\a -> hPutStrLn h $ unwords $ "add_cookie" : map quote a) $ cookiesArgs cookies
  hClose h
  void $ forkIO $ void $ waitForProcess pid
