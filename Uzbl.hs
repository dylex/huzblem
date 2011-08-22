{-# LANGUAGE FlexibleContexts #-}
module Uzbl
  ( Event(..)
  , EventHandler
  , UzblGlobal(..)
  , UzblClient(..), clientKey
  , UzblState(..), emptyState
  , UzblM
  , Key, Mod, ModKey
  , Prompt(..), Input

  , ask, modify, get
  , io
  , log, logPrint
  , run
  , getVar, setVar
  , resetVar, toggleVar, onOff
  , uzblURI, uri
  , newUzbl
  ) where

import Prelude hiding (log)

import Control.Concurrent (ThreadId, MVar)
import Control.Monad.Reader
import Control.Monad.State
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Word
import System.IO
import System.Posix.Types (ProcessID)

import Util
import Config
import Cookies

newtype Event = Event String deriving (Eq, Ord)

instance Show Event where
  showsPrec _ (Event e) = showString e
  show (Event e) = e

type EventHandler = [String] -> UzblM ()

data UzblGlobal = UzblGlobal
  { uzblemSocket :: !FilePath
  , uzblemClients :: MVar Clients
  , uzblCookies :: MVar Cookies
  }

data UzblClient = UzblClient
  { uzblGlobal :: !UzblGlobal
  , uzblThread :: !ThreadId
  , uzblHandle :: !Handle
  , uzblInstance :: !String
  , uzblPid :: !ProcessID
  , uzblEvent :: Maybe (Event, [String])
  }

type ClientKey = ProcessID
type Clients = Map.Map ClientKey UzblClient

clientKey :: UzblClient -> ClientKey
clientKey = uzblPid

type Key = String
type Mod = Word
type ModKey = (Mod, Key)

type Input = ([Char],String) -- zipper

data Prompt = Prompt
  { promptPrompt :: !String
  , promptInput :: !Input
  , promptExec :: String -> UzblM ()
  }

data UzblState = UzblState
  { uzblFIFO :: Maybe FilePath
  , uzblSocket :: Maybe FilePath
  , uzblVariables :: Config
  --uzblEvents :: Map.Map Event EventHandler
  , uzblBind :: ModKey -> UzblM ()
  , uzblPrompt :: Maybe Prompt
  }

emptyState :: UzblState
emptyState = UzblState
  { uzblFIFO = Nothing
  , uzblSocket = Nothing
  , uzblVariables = Map.empty
  --uzblEvents = Map.empty
  , uzblBind = const nop
  , uzblPrompt = Nothing
  }

type UzblT m = ReaderT UzblClient (StateT UzblState m)
type UzblM = UzblT IO

io :: IO a -> UzblM a
io = liftIO

log :: (MonadReader UzblClient m, MonadIO m) => String -> m ()
log s = do
  u <- ask
  liftIO $ putStrLn $ "huzbl " 
    ++ uzblInstance u 
    ++ "[" ++ show (uzblPid u) ++ "]"
    ++ maybe "" (\(e,a) -> ' ' : show e ++ ' ' : show a) (uzblEvent u)
    ++ if null s then "" else ':':' ':s

logPrint :: Show a => a -> UzblM ()
logPrint = log . show

run :: (MonadReader UzblClient m, MonadIO m) => String -> [String] -> m ()
run c a = do
  let s = unwords $ c : map quote a
  log $ "run " ++ s
  h <- uzblHandle =.< ask
  liftIO $ hPutStrLn h s

getVar :: Variable -> UzblM (Maybe Value)
getVar var = Map.lookup var . uzblVariables =.< get

setVar :: Variable -> Value -> UzblM ()
setVar var val = run ("set " ++ var ++ '=' : showValue val) []

resetVar :: Variable -> UzblM ()
resetVar var = maybe nop (setVar var) $ Map.lookup var defaultConfig

onOff :: [Value]
onOff = [ValInt 1, ValInt 0]

toggleVar :: Variable -> [Value] -> UzblM ()
toggleVar var vals = do
  x <- getVar var
  let i = fromMaybe (-1) $ (`elemIndex` init vals) =<< x
  setVar var (vals !! succ i)

uzblURI :: UzblM String
uzblURI = do
  v <- getVar "uri"
  return $ case v of 
    Just (ValString s) -> s
    _ -> ""

uri :: String -> UzblM ()
uri u = run ("uri " ++ escape u) []

newUzbl :: Maybe String -> UzblM ()
newUzbl u = do
  s <- uzblemSocket . uzblGlobal =.< ask
  c <- uzblVariables =.< get
  io $ runUzbl s c u
