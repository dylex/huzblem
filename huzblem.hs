import Prelude hiding (log)

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Function
import Data.IORef
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Network.Socket
import qualified System.Console.GetOpt as GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error

import Util
import Config
import Uzbl
import Bind
import Event
import Cookies

removeFile_ :: FilePath -> IO ()
removeFile_ f = void $ tryJust (\e -> guard (isDoesNotExistError e) >. ()) $ removeFile f

data Options = Options
  { optionSocket :: FilePath
  , optionConfig :: FilePath
  , optionElinksCookies :: Maybe FilePath
  }

defaultOptions :: Options
defaultOptions = Options
  { optionSocket = uzblHome ".huzblem"
  , optionConfig = uzblHome "config"
  , optionElinksCookies = Just $ home </> ".elinks/cookies"
  }

options :: [GetOpt.OptDescr (Options -> Options)]
options = 
  [ GetOpt.Option "s" ["socket"] 
      (GetOpt.ReqArg (\s o -> o{ optionSocket = if isAbsolute s then s else optionSocket o ++ '-' : s }) "PATH") 
      ("path or suffix for event manager socket [" ++ optionSocket defaultOptions ++ "]")
  , GetOpt.Option "c" ["config"]
      (GetOpt.ReqArg (\s o -> o{ optionConfig = s }) "FILE") 
      ("Path to config file [" ++ optionConfig defaultOptions ++ "]")
  , GetOpt.Option "e" ["elinks-cookies"]
      (GetOpt.OptArg (\s o -> o{ optionElinksCookies = s }) "FILE") 
      ("Load and use elinks cookies [" ++ fromMaybe "NONE" (optionElinksCookies defaultOptions) ++ "]")
  ]

main :: IO ()
main = do
  args <- getArgs
  (opts, urls) <- case GetOpt.getOpt GetOpt.Permute options args of
    (o, a, []) -> return (foldl' (flip ($)) defaultOptions o, a)
    (_, _, err) -> do
      mapM_ putStrLn err
      exitFailure

  s <- socket AF_UNIX Stream defaultProtocol
  let sock = optionSocket opts
      sa = SockAddrUnix sock
      ifdne e = guard dne >. dne where dne = isDoesNotExistError e

  me <- catchJust ifdne (do
      connect s sa
      sClose s
      putStrLn "huzblem already running"
      return False)
    (\dne -> do
      unless dne $ removeFile sock
      bindSocket s sa
      listen s (8+length args)
      return True)

  let uu [] = [Nothing]
      uu l = map Just l
  mapM_ (runUzbl sock defaultConfig) (uu urls)

  unless me exitSuccess

  sem <- newIORef (1 :: Int)
  wait <- newQSem 0
  let up = atomicModifyIORef sem (\i -> (succ i, ()))
      down = do
        i <- atomicModifyIORef sem (\i -> (pred i, pred i))
        when (i == 0) $ signalQSem wait

  clients <- newMVar Map.empty
  cookies <- newMVar =<< maybe (return emptyCookies) loadElinksCookies (optionElinksCookies opts)
  let global = UzblGlobal
        { uzblemSocket = sock
        , uzblemClients = clients
        , uzblCookies = cookies
        }

  void $ forkIO $ forever $ accept s >>= \r -> do
    void $ forkIO $ bracket_ up down (client global r)
  void $ forkIO $ do
    threadDelay 5000000
    down
  waitQSem wait
  removeFile_ sock

client :: UzblGlobal -> (Socket, SockAddr) -> IO ()
client global (s,_) = do
  h <- socketToHandle s ReadWriteMode
  hSetBuffering h LineBuffering
  l <- hGetLine h
  case words l of
    ["EVENT", '[':sinst, "INSTANCE_START", spid]
      | Just inst <- stripLast ']' sinst
      , [(pid,"")] <- reads spid -> do
        tid <- myThreadId
        let c = UzblClient
              { uzblGlobal = global
              , uzblThread = tid
              , uzblHandle = h
              , uzblInstance = inst
              , uzblPid = pid
              , uzblEvent = Nothing
              }
            ucl = modifyMVar_ $ uzblemClients global
        bracket_ 
          (ucl $ return . Map.insert (clientKey c) c)
          (ucl $ return . Map.update (\c' -> guard (on (/=) uzblThread c c') >. c') (clientKey c))
          (evalStateT (runReaderT (proc c) c) emptyState)
    _ -> putStrLn $ "huzblem: bad start: " ++ l

proc :: UzblClient -> UzblM ()
proc c = do
  let tpl = "EVENT [" ++ uzblInstance c ++ "] "
      loop = (`when` loop) =<< line =<< io (hGetLine (uzblHandle c))
      line "" = return True
      line s
        | Just l <- stripPrefix tpl s
        , ev:args <- quotedWords l = local (\ur -> ur{ uzblEvent = Just (Event ev, args) }) $
            if ev == "INSTANCE_EXIT"
              then log "finished" >. False
              else log "" >> event (Event ev) args >. True
        | otherwise = log s >. True
  log "starting"
  cookies <- io . readMVar $ uzblCookies $ uzblGlobal c
  mapM_ (run "add_cookie") $ cookiesArgs cookies
  defaultMode
  loop

