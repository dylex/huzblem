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

import Util
import Config
import Uzbl
import Event
import Cookies
import Database
import Block
import URIs

removeFile_ :: FilePath -> IO ()
removeFile_ f = void $ tryJust (\e -> guard (isDoesNotExistError e) >. ()) $ removeFile f

data Options = Options
  { optionSocket :: Maybe String
  , optionCookies :: Maybe FilePath
  , optionDebug :: Bool
  , optionConfig :: Config
  , optionBlocks :: FilePath
  , optionDatabase :: Maybe String
  }

defaultOptions :: Options
defaultOptions = Options
  { optionSocket = Nothing
  , optionCookies = Just $ uzblHome "cookies.txt"
  , optionDebug = False
  , optionConfig = defaultConfig
  , optionBlocks = uzblHome "block"
  , optionDatabase = Just ""
  }

defaultSocket :: FilePath
defaultSocket = uzblHome ".huzblem"

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
      (GetOpt.ReqArg (\s o -> o{ optionSocket = Just s }) "NAME|PATH") 
      ("suffix or absolute path for event manager socket [" ++ defaultSocket ++ "]")
  , GetOpt.Option "c" ["cookies"]
      (GetOpt.OptArg (\s o -> o{ optionCookies = s }) "FILE") 
      ("Load and use cookies from FILE [" ++ fromMaybe "NONE" (optionCookies defaultOptions) ++ "]")
  , GetOpt.Option "v" ["verbose"]
      (GetOpt.NoArg (\o -> o{ optionDebug = True }))
      ("Print out more log messages")
  , GetOpt.Option "s" ["set"]
      (GetOpt.ReqArg setConfig "VAR[=VALUE]")
      ("Set (or clear) a configuration variable")
  , GetOpt.Option "p" ["private"]
      (GetOpt.NoArg (optionConfig' $ Map.insert "enable_private" (ValInt 1)))
      ("Private mode (equivalent to -s enable_private=1)")
  , GetOpt.Option "d" ["database"]
      (GetOpt.ReqArg (\s o -> o{ optionDatabase = Just s }) "CONN")
      ("database connection info [" ++ fromJust (optionDatabase defaultOptions) ++ "]")
  , GetOpt.Option "n" ["no-database"]
      (GetOpt.NoArg (\o -> o{ optionDatabase = Nothing }))
      "do not connect to a database"
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
  path <- lookupEnv "PATH"
  setEnv "PATH" (uzblHome "" ++ maybe "" (':':) path)
  hSetEncoding stdout char8

  s <- socket AF_UNIX Stream defaultProtocol
  let sock = maybe defaultSocket (\p -> if isAbsolute p then p else defaultSocket ++ '-' : p) $ optionSocket opts
      sa = SockAddrUnix sock
      catchdne f h = catchJust (\e -> guard (isDoesNotExistError e) >. ()) f (\() -> h)

  me <- catchdne (do
      connect s sa
      close s
      putStrLn "huzblem already running"
      return False)
    (do
      removeFile_ sock
      bind s sa
      listen s (8+length args)
      return True)

  cookies <- case optionCookies opts of
    Nothing -> return emptyCookies
    Just f 
      | Just f' <- stripPrefix "elinks:" f -> loadElinksCookies f'
      | ".elinks/" `isInfixOf` f -> loadElinksCookies f
      | otherwise -> loadCookiesTxt f

  let uu [] = [Nothing]
      uu l = map (Just . expandURI) l
  mapM_ (runUzbl sock cookies (optionConfig opts) (optionDebug opts)) (uu urls)

  unless me exitSuccess

  clients <- newMVar Map.empty
  db <- databaseOpen (optionDatabase opts)
  blocks <- (newMVar $!) =<< catchdne (loadBlocks (optionBlocks opts)) (return defaultBlocks)
  scriptinit <- newIORef (error "scripts uninit")
  let global = UzblGlobal
        { uzblemSocket = sock
        , uzblemClients = clients
        , uzblemCookies = cookies
        , uzblDatabase = db
        , uzblDebug = optionDebug opts
        , uzblBlocks = blocks
        , uzblScriptInit = scriptinit
        }
  setScriptInit global

  sem <- newIORef (1 :: Int)
  wait <- newQSem 0
  let up = atomicModifyIORef sem (\i -> (succ i, ()))
      down = do
        i <- atomicModifyIORef sem (join (,) . pred)
        when (i == 0) $ signalQSem wait

  void $ forkIO $ forever $ accept s >>= \r ->
    void $ forkIO $ bracket_ up down (client global r)
  void $ forkIO $ do
    threadDelay 5000000
    down
  waitQSem wait

  when (isNothing (optionSocket opts)) $
    saveBlocks (optionBlocks opts) =<< takeMVar blocks
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
          (evalStateT (runReaderT (proc c) c) emptyState)
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

