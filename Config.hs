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

import Util

type Variable = String

data Value 
  = ValInt Int
  | ValFloat Float
  | ValString String
  deriving (Eq)

type Config = Map.Map Variable Value

readsValue :: String -> ReadS Value
readsValue "int" s = map (first ValInt) $ reads s
readsValue "float" s = map (first ValFloat) $ reads s
readsValue "str" s = [(ValString s,"")]
readsValue _ _ = []

readValue :: String -> String -> Maybe Value
readValue t s = case readsValue t s of
  [(v,"")] -> Just v
  _ -> Nothing

showValue :: Value -> String
showValue (ValInt i) = show i
showValue (ValFloat f) = show f
showValue (ValString s) = s

instance Show Value where
  showsPrec p (ValInt x) = showsPrec p x
  showsPrec p (ValFloat x) = showsPrec p x
  showsPrec p (ValString x) = showsPrec p x


home :: FilePath
home = Unsafe.unsafeDupablePerformIO $ getEnv "HOME"

uzblHome :: FilePath -> FilePath
uzblHome = ((home </> ".uzbl") </>)

stylesheets :: [Value]
stylesheets = map (ValString . ("file://" ++) . uzblHome) ["plain.css", "style.css"]

baseConfig :: Config
baseConfig = Map.fromAscList
  [ ("fifo_dir",		ValString $ uzblHome "")
  , ("socket_dir",		ValString $ uzblHome "")
  , ("status_background",	ValString "#AAA")
  , ("status_format",		ValString "\\@status_prompt<span color='#440'>\\@[\\@SELECTED_URI]\\@</span>")
  , ("status_format_right",	ValString "<span color='#048'>\\@[\\@uri]\\@</span>")
  , ("title_format_long",	ValString "\\@TITLE - Uzbl <\\@NAME> \\@SELECTED_URI")
  , ("title_format_short",	ValString "uzbl \\@TITLE")
  , ("uzbl_home",		ValString $ uzblHome "")
  ]

defaultConfig :: Config
defaultConfig = Map.union (Map.fromAscList 
  [ ("caret_browsing",		ValInt 1)
  , ("shell_cmd",		ValString $ uzblHome "sh")
  , ("show_status",		ValInt 1)
  , ("status_top",		ValInt 0)
  , ("stylesheet_uri",		head stylesheets)
  , ("useragent",		ValString $ uzblHome "Uzbl (Webkit @{WEBKIT_MAJOR}.@{WEBKIT_MINOR}) (@(+uname -sm)@ [@ARCH_UZBL])")
  , ("zoom_type",		ValInt 0)
  ]) baseConfig

runUzbl :: FilePath -> Config -> Maybe String -> IO ()
runUzbl sock config uri = do
  let args = ["--connect-socket", sock, "--config", "-"] ++ maybe [] (("-u":) . return) uri
  print args
  (Just h, _, _, pid) <- createProcess (proc "uzbl-core" args){ std_in = CreatePipe }
  mapM_ (\(k,v) -> hPutStrLn h $ "set " ++ k ++ '=' : showValue v) $ Map.toList $ Map.union baseConfig config
  hClose h
  void $ forkIO $ void $ waitForProcess pid
