{-# LANGUAGE FlexibleContexts #-}
module Uzbl
  ( Event(..)
  , UzblGlobal(..)
  , UzblClient(..), clientKey
  , UzblState(..), emptyState
  , UzblM
  , Bindings(..), Input

  , ask, modify, get, put
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
  , updateBlockScript
  ) where

import Prelude hiding (log)

import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.State
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Sequence as Seq
import System.IO
import System.Posix.Types (ProcessID)

import Safe

import Util
import Config
import Database
import Cookies
import Scripts
import URIs

newtype Event = Event String deriving (Eq, Ord)

instance Show Event where
  showsPrec _ (Event e) = showString e
  show (Event e) = e

type ClientKey = ProcessID
type Clients = Map.Map ClientKey UzblClient

type Input = ([Char],String) -- zipper

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
    , promptCompletion :: String -> UzblM (Maybe String)
    , promptExec :: Maybe String -> UzblM ()
    }

data UzblGlobal = UzblGlobal
  { uzblemSocket :: !FilePath
  , uzblemClients :: MVar Clients
  , uzblemCookies :: Cookies
  , uzblDatabase :: Database
  , uzblDebug :: Bool
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
  , uzblBlockScript :: Script -- cached
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
  , uzblBlockScript = ""
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
  d <- uzblDebug . uzblGlobal =.< ask
  when d $ log s

logPrint :: Show a => a -> UzblM ()
logPrint = log . show

run :: (MonadReader UzblClient m, MonadIO m) => String -> m ()
run s = do
  debug $ "run " ++ s
  h <- uzblHandle =.< ask
  liftIO $ hPutStrLn h s

runArgs :: (MonadReader UzblClient m, MonadIO m) => String -> [String] -> m ()
runArgs c a = run $ unwords $ c : map quote a

runOthers :: String -> [String] -> UzblM ()
runOthers r a = ask >>= \ct -> liftIO $
  mapM_ (runReaderT $ runArgs r a) . Map.elems . Map.delete (clientKey ct) =<< 
    readMVar (uzblemClients (uzblGlobal ct))

getVar :: Variable -> UzblM Value
getVar var = fromMaybe ValNone . Map.lookup var . uzblVariables =.< get

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
setVar var val = run $ "set " ++ var ++ '=' : showValue val

setVarMsg :: Variable -> Value -> UzblM ()
setVarMsg var val = do
  status c
  run $ "set " ++ c
  where c = var ++ '=' : showValue val

resetVar :: Variable -> UzblM ()
resetVar var = maybe nop (setVar var) $ Map.lookup var defaultConfig

onOff :: [Value]
onOff = [ValInt 1, ValInt 0]

toggleVar :: Variable -> [Value] -> UzblM ()
toggleVar var vals = do
  x <- getVar var
  let y = (!!) vals $ succ $ fromMaybe (-1) $ elemIndex x $ init vals
  setVarMsg var y

uzblURI :: UzblM String
uzblURI = getVarStr "uri"

goto :: String -> UzblM ()
goto u = run $ "set uri=" ++ escape (expandURI u)

status :: String -> UzblM ()
status "" = setVar "status_message" $ ValStr ""
status x = setVar "status_message" $ ValStr $ "<span color='#404'>" ++ mlEscape x ++ "</span> "

modifyBindings :: (Bindings -> Bindings) -> UzblM ()
modifyBindings f = modify $ \u -> u{ uzblBindings = f $ uzblBindings u }

withDatabase :: (Database -> IO a) -> UzblM a
withDatabase f = io . f . uzblDatabase . uzblGlobal =<< ask

newUzbl :: Maybe String -> UzblM ()
newUzbl uri = do
  c <- ask
  u <- get
  io $ runUzbl 
    (uzblemSocket $ uzblGlobal c) 
    (uzblCookies u) 
    (Map.insert "parent" (ValInt $ fromIntegral $ clientKey c) $ uzblVariables u) 
    uri

blockScript :: UzblM Script
blockScript = do
  bl <- withDatabase blockLists
  bm <- mapM (\t -> ((,) t) . toEnum =.< getVarInt ("block_" ++ t)) bc
  bv <- getVarInt "block_verbose"
  return $ scriptBlock (0 /= bv) bl $ bm ++ map (\t -> (t, BlockNone)) ba
  where 
    bc = ["iframe","img","script","embed"]
    ba = ["input","frame","link"]

updateBlockScript :: UzblM ()
updateBlockScript = do
  s <- blockScript
  modify $ \u -> u{ uzblBlockScript = s }

