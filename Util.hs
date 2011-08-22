module Util 
  ( nop
  , (>.), (>.=), (>=.)
  , (=.<), (.=<)

  , first, second

  , stripLast
  , splitOn
  , quotedWord, quotedWords
  , escape, quote
  , mlEscape, uriEscape

  , capture, pipe
  ) where

import Control.Monad
import Data.Char
import System.Exit
import System.IO
import System.Process

nop :: Monad m => m ()
nop = return ()

infixl 1 >., >.=, >=.
infixr 1 =.<, .=<
(>.) :: Monad m => m a -> b -> m b
(>.=) :: Monad m => m a -> (a -> b) -> m b
(=.<) :: Monad m => (a -> b) -> m a -> m b
(>=.) :: Monad m => (a -> m b) -> (b -> c) -> a -> m c
(.=<) :: Monad m => (b -> c) -> (a -> m b) -> a -> m c

(>.) e r = e >> return r
(>.=) e r = e >>= return . r
(=.<) r e = return . r =<< e -- fmap, <$>, liftM
(>=.) e r = e >=> return . r
(.=<) r e = return . r <=< e

-- Control.Arrow
first  :: (a -> b) -> (a, c) -> (b, c)
second :: (b -> c) -> (a, b) -> (a, c)
first  f (a, b) = (f a, b)
second f (a, b) = (a, f b)

stripLast :: Eq a => a -> [a] -> Maybe [a]
stripLast x [y] | x == y = Just []
stripLast x (c:l) = fmap (c:) $ stripLast x l
stripLast _ _ = Nothing

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn f l = case break f l of
  (s,_:l') -> s : splitOn f l'
  (s,[])  -> [s]

quotedWord :: String -> (String, String)
quotedWord [] = ("", "")
quotedWord (q:s) | q `elem` "\'\"" = ps s where
  ps ('\\':c:s') = pc c s'
  ps (c:s') 
    | c == q = ("", s')
    | otherwise = pc c s'
  ps [] = ([], []) -- XXX premature termination
  pc c = first (c:) . ps
quotedWord s = break isSpace s

quotedWords :: String -> [String]
quotedWords [] = []
quotedWords (c:s) | isSpace c = quotedWords s
quotedWords s = w : quotedWords r where (w,r) = quotedWord s

quote :: String -> String
quote = show

escape :: String -> String
escape [] = []
escape (c:s)
  | c `elem` "@" = '\\':s'
  | otherwise = s'
  where s' = c:escape s

mlEscape :: String -> String
mlEscape "" = ""
mlEscape ('@':s) = "\\@" ++ mlEscape s
mlEscape ('&':s) = "&amp;" ++ mlEscape s
mlEscape ('<':s) = "&lt;" ++ mlEscape s
mlEscape ('>':s) = "&gt;" ++ mlEscape s
mlEscape ('\'':s) = "&apos;" ++ mlEscape s
mlEscape ('"':s) = "&quot;" ++ mlEscape s
mlEscape (c:s)
  |    (0x01 <= i && i <= 0x08) 
    || (0x0b <= i && i <= 0x0c) 
    || (0x0e <= i && i <= 0x1f) 
    || (0x7f <= i && i <= 0x84) 
    || (0x86 <= i && i <= 0x9f)
    = "&#" ++ show i ++ ';' : mlEscape s
  | otherwise = c : mlEscape s 
  where i = ord c

uriEscape :: String -> String
uriEscape _ = error "uriEscape: TODO"

capture :: FilePath -> [String] -> IO (Maybe String)
capture cmd args = do
  (_, Just h, _, pid) <- createProcess (proc cmd args){ std_out = CreatePipe }
  out <- hGetContents h
  r <- waitForProcess pid
  return $ case r of
    ExitSuccess -> Just out
    _ -> Nothing

pipe :: FilePath -> [String] -> String -> IO ()
pipe cmd args str = do
  (Just h, _, _, pid) <- createProcess (proc cmd args){ std_in = CreatePipe }
  hPutStr h str
  hClose h
  void $ waitForProcess pid
