import Data.Char

import Safe

import Util

data Mode
  = Sym
  | Word
  | Quote !Char
  | CommentLine
  | CommentBlock
  deriving (Eq, Ord)

jss :: String -> String
jss = flip p Sym where
  p [] (Quote _) = error "unterminated quote"
  p [] CommentBlock = error "unterminated comment"
  p [] _ = []
  p ('\n':s) CommentLine = p s Sym
  p ('*':'/':s) CommentBlock = p s Sym
  p (_:s) CommentLine = p s CommentLine
  p (_:s) CommentBlock = p s CommentBlock
  p ('\\':c:s) m@(Quote q) | c == q || c == '\\' = '\\':c:p s m
  p ('\'':s) m = '\'':pq '\'' s m
  p ('\"':s) m = '\"':pq '\"' s m
  p ('/':'/':s) m | m <= Word = p s CommentLine
  p ('/':'*':s) m | m <= Word = p s CommentBlock
  p (c:s) m 
    | isSpace c = case m of
      Word | isAlphaNum (headDef ' ' s) -> ' ':p s Sym
      Quote _ 
        | c /= '\n' && c /= '\r' -> c:p s m
        | otherwise -> error "newline in quote"
      _ -> p s m
    | otherwise = c:p s (case m of
      Quote _ -> m
      _ | isAlphaNum c -> Word 
        | otherwise -> Sym)
  pq c s m@(Quote q)
    | q == c = p s Sym
    | otherwise = p s m
  pq c s _ = p s (Quote c)

main :: IO ()
main = putStr . escape . jss =<< getContents
