import Prelude hiding (log)

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Char
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
import System.Posix.Env as Env

import Util
import Config
import Uzbl
import Event
import Cookies
import Database
import URIs

removeFile_ :: FilePath -> IO ()
removeFile_ f = void $ tryJust (\e -> guard (isDoesNotExistError e) >. ()) $ removeFile f

data Options = Options
  { optionSocket :: FilePath
  , optionCookies :: Maybe FilePath
  , optionDebug :: Bool
  , optionConfig :: Config
  }

defaultOptions :: Options
defaultOptions = Options
  { optionSocket = uzblHome ".huzblem"
  , optionCookies = Just $ uzblHome "cookies" -- home </> ".elinks/cookies"
  , optionDebug = False
  , optionConfig = defaultConfig
  }

optionConfig' :: (Config -> Config) -> Options -> Options
optionConfig' f o = o{ optionConfig = f (optionConfig o) }

setConfig :: String -> Options -> Options
setConfig c = case break ('=' ==) c of
  ("","") -> id
  (v,'=':s) | Just x <- readValue "" s -> optionConfig' $ Map.insert v x
  (v,_) -> optionConfig' $ Map.delete v

options :: [GetOpt.OptDescr (Options -> Options)]
options = 
  [ GetOpt.Option "s" ["socket"] 
      (GetOpt.ReqArg (\s o -> o{ optionSocket = if isAbsolute s then s else optionSocket o ++ '-' : s }) "PATH") 
      ("path or suffix for event manager socket [" ++ optionSocket defaultOptions ++ "]")
  , GetOpt.Option "" ["cookies"]
      (GetOpt.OptArg (\s o -> o{ optionCookies = s }) "FILE") 
      ("Load and use cookies from FILE [" ++ fromMaybe "NONE" (optionCookies defaultOptions) ++ "]")
  , GetOpt.Option "d" ["debug"]
      (GetOpt.NoArg (\o -> o{ optionDebug = True }))
      ("Print out more log messages")
  , GetOpt.Option "s" ["set"]
      (GetOpt.ReqArg setConfig "VAR[=VALUE]")
      ("Set (or clear) a configuration variable")
  , GetOpt.Option "p" ["private"]
      (GetOpt.NoArg (optionConfig' $ Map.insert "enable_private" (ValInt 1)))
      ("Private mode (equivalent to -s enable_private=1)")
  ]

main :: IO ()
main = do
  args <- getArgs
  (opts, urls) <- case GetOpt.getOpt GetOpt.Permute options args of
    (o, a, []) -> return (foldl' (flip ($)) defaultOptions o, a)
    (_, _, err) -> do
      mapM_ putStr err
      putStr $ GetOpt.usageInfo "huzblem [OPTIONS] [URI ...]" options
      exitFailure
  path <- Env.getEnv "PATH"
  setEnv "PATH" (uzblHome "" ++ maybe "" (':':) path) True
  hSetEncoding stdout latin1

  s <- socket AF_UNIX Stream defaultProtocol
  let sock = optionSocket opts
      sa = SockAddrUnix sock
      ifdne e = guard (isDoesNotExistError e) >. ()

  me <- catchJust ifdne (do
      connect s sa
      sClose s
      putStrLn "huzblem already running"
      return False)
    (\() -> do
      removeFile_ sock
      bindSocket s sa
      listen s (8+length args)
      return True)

  cookies <- case optionCookies opts of
    Nothing -> return emptyCookies
    Just f 
      | ".elinks/" `isInfixOf` f -> loadElinksCookies f
      | otherwise -> loadCookies f

  let uu [] = [Nothing]
      uu l = map (Just . expandURI) l
  mapM_ (runUzbl sock cookies (optionConfig opts)) (uu urls)

  unless me exitSuccess

  sem <- newIORef (1 :: Int)
  wait <- newQSem 0
  let up = atomicModifyIORef sem (\i -> (succ i, ()))
      down = do
        i <- atomicModifyIORef sem (\i -> (pred i, pred i))
        when (i == 0) $ signalQSem wait

  clients <- newMVar Map.empty
  db <- databaseOpen
  let global = UzblGlobal
        { uzblemSocket = sock
        , uzblemClients = clients
        , uzblemCookies = cookies
        , uzblDatabase = db
        , uzblDebug = optionDebug opts
        }

  void $ forkIO $ forever $ accept s >>= \r -> do
    void $ forkIO $ bracket_ up down (client global r)
  void $ forkIO $ do
    threadDelay 5000000
    down
  waitQSem wait
  removeFile_ sock
  databaseClose db

client :: UzblGlobal -> (Socket, SockAddr) -> IO ()
client global (s,_) = do
  h <- socketToHandle s ReadWriteMode
  hSetBuffering h LineBuffering
  hSetEncoding h utf8
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
          (evalStateT (runReaderT (proc c) c) emptyState{
              uzblCookies = uzblemCookies global
            })
    _ -> putStrLn $ "huzblem: bad start: " ++ l

proc :: UzblClient -> UzblM ()
proc c = do
  let evt = "EVENT [" ++ uzblInstance c ++ "] "
      rqt = "REQUEST [" ++ uzblInstance c ++ "] "
      loop = (`when` loop) =<< line =<< io (hGetLine (uzblHandle c))
      line "" = return True
      line s
        | Just l <- stripPrefix evt s
        , ev:args <- quotedWords l = go (Event ev) args
        | Just l <- stripPrefix rqt s
        , (rq,arg) <- breakStrip isSpace l = go (Request rq) [arg]
        | otherwise = log s >. True
      go ev args = local (\ur -> ur{ uzblEvent = Just (ev, args) }) $
        if ev == Event "INSTANCE_EXIT"
          then log "finished" >. False
          else debug "" >> event ev args >. True
  log "starting"
  loop

