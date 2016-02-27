{-# LANGUAGE FlexibleContexts #-}
module Uzbl
  ( Event(..)
  , UzblGlobal(..)
  , UzblClient(..), clientKey
  , UzblState(..), emptyState
  , UzblM
  , Bindings(..)
  , Input, Completer

  , ask, asks, modify, get, gets, put
  , io
  , log, logPrint, debug
  , run, runArgs, runOthers
  , getVar, getVarInt, getVarStr
  , setVar', setVar, setVarMsg
  , resetVar, toggleVar, onOff
  , uzblURI, goto
  , status
  , modifyBindings
  , withDatabase
  , newUzbl
  , setScriptInit, updateScriptInit
  , runScript
  , runScriptInit
  , request
  ) where

import Prelude hiding (log)

import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.State
import Data.IORef
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.Time
import System.IO
import System.Posix.Types (ProcessID)

import Safe

import Util
import Config
import Database
import Cookies
import Scripts
import URIs
import Block
import Keys (ModKey)

data Event 
  = Event String
  | Request String
  deriving (Eq, Ord)

instance Show Event where
  show (Event e) = e
  show (Request e) = "REQUEST:" ++ e

type ClientKey = ProcessID
type Clients = Map.Map ClientKey UzblClient

type Input = ([Char],String) -- zipper

type Completer = String -> UzblM (Maybe String)

data Bindings
  = Command
    { commandCount :: Maybe Int
    }
  | PassThrough
    { bindingsReturn :: Bindings 
    }
  | Prompt 
    { promptPrompt :: !String
    , promptInput :: !Input
    , promptCompleter :: Completer
    , promptExec :: Maybe String -> UzblM ()
    }
  | Capture
    { captureFun :: ModKey -> UzblM ()
    , bindingsReturn :: Bindings 
    }

data UzblGlobal = UzblGlobal
  { uzblemSocket :: !FilePath
  , uzblemClients :: MVar Clients
  , uzblemCookies :: Cookies
  , uzblDatabase :: Database
  , uzblDebug :: Bool
  , uzblBlocks :: MVar Blocks
  , uzblScriptInit :: IORef String
  }

data UzblClient = UzblClient
  { uzblGlobal :: !UzblGlobal
  , uzblThread :: !ThreadId
  , uzblHandle :: !Handle
  , uzblInstance :: !String
  , uzblPid :: !ProcessID
  , uzblEvent :: Maybe (Event, [String])
  }

data UzblState = UzblState
  { uzblFIFO :: Maybe FilePath
  , uzblSocket :: Maybe FilePath
  , uzblVariables :: Config
  , uzblCookies :: Cookies
  , uzblBindings :: Bindings
  , uzblPromptHistory :: Seq.Seq String
  , uzblLastLoad :: Maybe (Bool, Data.Time.UTCTime)
  }

type UzblT m = ReaderT UzblClient (StateT UzblState m)
type UzblM = UzblT IO

io :: IO a -> UzblM a
io = liftIO

emptyState :: UzblState
emptyState = UzblState
  { uzblFIFO = Nothing
  , uzblSocket = Nothing
  , uzblVariables = Map.empty
  --uzblEvents = Map.empty
  , uzblCookies = emptyCookies
  , uzblBindings = Command
    { commandCount = Nothing
    }
  , uzblPromptHistory = Seq.empty
  , uzblLastLoad = Nothing
  }

clientKey :: UzblClient -> ClientKey
clientKey = uzblPid

log :: (MonadReader UzblClient m, MonadIO m) => String -> m ()
log s = do
  u <- ask
  liftIO $ putStrLn $ "huzbl " 
    ++ uzblInstance u 
    ++ "[" ++ show (uzblPid u) ++ "]"
    ++ maybe "" (\(e,a) -> ' ' : show e ++ ' ' : show a) (uzblEvent u)
    ++ if null s then "" else ':':' ':s

debug :: (MonadReader UzblClient m, MonadIO m) => String -> m ()
debug s = do
  d <- asks (uzblDebug . uzblGlobal)
  when d $ log s

logPrint :: Show a => a -> UzblM ()
logPrint = log . show

run :: (MonadReader UzblClient m, MonadIO m) => String -> m ()
run s = do
  debug $ "run " ++ s
  h <- asks uzblHandle
  liftIO $ hPutStrLn h s

runArgs :: (MonadReader UzblClient m, MonadIO m) => String -> [String] -> m ()
runArgs c a = run $ unwords $ c : map quote a

runOthers :: String -> [String] -> UzblM ()
runOthers r a = ask >>= \ct -> liftIO $
  mapM_ (runReaderT $ runArgs r a) . Map.elems . Map.delete (clientKey ct) =<< 
    readMVar (uzblemClients (uzblGlobal ct))

getVar :: Variable -> UzblM Value
getVar var = gets $ fromMaybe ValNone . Map.lookup var . uzblVariables

getVarStr :: Variable -> UzblM String
getVarStr var = showValue =.< getVar var

getVarInt :: Variable -> UzblM Int
getVarInt var = fromMaybe 0 . vi =.< getVar var where
  vi ValNone = Nothing
  vi (ValInt i) = Just i
  vi (ValStr s) = readMay s
  vi (ValFloat f) = Just $ truncate f

setVar' :: Variable -> Value -> UzblM ()
setVar' var val = modify $ \u -> u{ uzblVariables = Map.insert var val (uzblVariables u) }

setVar :: Variable -> Value -> UzblM ()
setVar var val = run $ "set " ++ var ++ ' ' : showValue val

setVarMsg :: Variable -> Value -> UzblM ()
setVarMsg var val = do
  status c
  run $ "set " ++ c
  where c = var ++ ' ' : showValue val

resetVar :: Variable -> UzblM ()
resetVar var = maybe nop (setVar var) $ Map.lookup var defaultConfig

onOff :: [Value]
onOff = [ValInt 1, ValInt 0]

toggleVar :: Variable -> [Value] -> UzblM Value
toggleVar var vals = do
  x <- getVar var
  let y = (!!) vals $ succ $ fromMaybe (-1) $ elemIndex x $ init vals
  setVarMsg var y
  return y

uzblURI :: UzblM String
uzblURI = getVarStr "uri"

goto :: String -> UzblM ()
goto u = run $ "uri " ++ escape (expandURI u)

status :: String -> UzblM ()
status "" = setVar "status_message" $ ValStr ""
status x = setVar "status_message" $ ValStr $ "<span color='#404'>" ++ mlEscape x ++ "</span> "

modifyBindings :: (Bindings -> Bindings) -> UzblM ()
modifyBindings f = modify $ \u -> u{ uzblBindings = f $ uzblBindings u }

withDatabase :: (Database -> IO a) -> UzblM a
withDatabase f = io . f =<< asks (uzblDatabase . uzblGlobal)

newUzbl :: Maybe String -> UzblM ()
newUzbl uri = do
  c <- ask
  u <- get
  io $ runUzbl 
    (uzblemSocket $ uzblGlobal c) 
    (uzblCookies u) 
    (Map.insert "parent" (ValInt $ fromIntegral $ clientKey c) $ uzblVariables u) 
    (uzblDebug $ uzblGlobal c)
    (fmap expandURI uri)

runScript :: String -> UzblM ()
runScript s = runArgs "js" ["page","string", s ++ "undefined"]

setScriptInit :: UzblGlobal -> IO ()
setScriptInit g =
  writeIORef (uzblScriptInit g) . scriptSetBlocks =<< readMVar (uzblBlocks g)

updateScriptInit :: UzblM ()
updateScriptInit = io . setScriptInit =<< asks uzblGlobal

runScriptInit :: String -> UzblM ()
runScriptInit r = do
  si <- io . readIORef . uzblScriptInit =<< asks uzblGlobal
  debug $ "init " ++ r
  runArgs "js" ["page","file","init.min.js"]
  runScript (si ++ r)

request :: String -> String -> UzblM ()
request t s = do
  i <- asks uzblInstance
  run $ "js " ++ scriptRequest i t s
