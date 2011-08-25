{-# LANGUAGE FlexibleContexts #-}
module Uzbl
  ( Event(..)
  , EventHandler
  , UzblGlobal(..)
  , UzblClient(..), clientKey
  , UzblState(..), emptyState
  , UzblM
  , Prompt(..), Input

  , ask, modify, get, put
  , io
  , log, logPrint, debug
  , run, runOthers
  , getVar, getVarInt, getVarStr
  , setVar
  , resetVar, toggleVar, onOff
  , uzblURI, goto
  , newUzbl
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
import Keys
import Cookies
import URIs

newtype Event = Event String deriving (Eq, Ord)

instance Show Event where
  showsPrec _ (Event e) = showString e
  show (Event e) = e

type EventHandler = [String] -> UzblM ()

data UzblGlobal = UzblGlobal
  { uzblemSocket :: !FilePath
  , uzblemClients :: MVar Clients
  , uzblCookies :: MVar Cookies
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

type ClientKey = ProcessID
type Clients = Map.Map ClientKey UzblClient

clientKey :: UzblClient -> ClientKey
clientKey = uzblPid

type Input = ([Char],String) -- zipper

data Prompt = Prompt
  { promptPrompt :: !String
  , promptInput :: !Input
  , promptExec :: Maybe String -> UzblM ()
  }

data UzblState = UzblState
  { uzblFIFO :: Maybe FilePath
  , uzblSocket :: Maybe FilePath
  , uzblVariables :: Config
  --uzblEvents :: Map.Map Event EventHandler
  , uzblBind :: ModKey -> UzblM ()
  , uzblPrompt :: Maybe Prompt
  , uzblPromptHistory :: Seq.Seq String
  }

emptyState :: UzblState
emptyState = UzblState
  { uzblFIFO = Nothing
  , uzblSocket = Nothing
  , uzblVariables = Map.empty
  --uzblEvents = Map.empty
  , uzblBind = const nop
  , uzblPrompt = Nothing
  , uzblPromptHistory = Seq.empty
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

debug :: (MonadReader UzblClient m, MonadIO m) => String -> m ()
debug s = do
  d <- uzblDebug . uzblGlobal =.< ask
  when d $ log s

logPrint :: Show a => a -> UzblM ()
logPrint = log . show

run :: (MonadReader UzblClient m, MonadIO m) => String -> [String] -> m ()
run c a = do
  let s = unwords $ c : map quote a
  debug $ "run " ++ s
  h <- uzblHandle =.< ask
  liftIO $ hPutStrLn h s

runOthers :: String -> [String] -> UzblM ()
runOthers r a = ask >>= \ct -> liftIO $
  mapM_ (runReaderT $ run r a) . Map.elems . Map.delete (clientKey ct) =<< 
    readMVar (uzblemClients (uzblGlobal ct))

getVar :: Variable -> UzblM (Maybe Value)
getVar var = Map.lookup var . uzblVariables =.< get

getVarStr :: Variable -> UzblM String
getVarStr var = maybe "" showValue =.< getVar var

getVarInt :: Variable -> UzblM Int
getVarInt var = do
  v <- getVar var
  return $ fromMaybe 0 $ vi =<< v
  where
    vi (ValInt i) = Just i
    vi (ValStr s) = readMay s
    vi (ValFloat f) = Just $ truncate f

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
uzblURI = getVarStr "uri"

goto :: String -> UzblM ()
goto u = run ("set uri=" ++ escape (expandURI u)) []

newUzbl :: Maybe String -> UzblM ()
newUzbl u = do
  g <- uzblGlobal =.< ask
  c <- io $ readMVar $ uzblCookies g
  v <- uzblVariables =.< get
  io $ runUzbl (uzblemSocket g) c v u
