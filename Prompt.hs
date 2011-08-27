module Prompt
  ( prompt
  ) where

import Data.Char
import Data.List
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Data.Tuple

import Safe

import Util
import Config
import Keys
import Uzbl

historyLength :: Int
historyLength = 255

input :: Input -> String
input (il, ir) = reverse il ++ ir

unInput :: String -> Input
unInput s = (reverse s, "")

promptStop :: UzblM ()
promptStop = do
  u@UzblState{ uzblPrompt = Just p } <- get
  put u{ uzblPrompt = Nothing }
  promptExec p Nothing

promptRun :: UzblM ()
promptRun = do
  u@UzblState{ uzblPrompt = Just p } <- get
  let s = input (promptInput p)
  put $ historyPush s u{ uzblPrompt = Nothing }
  promptExec p $ Just s

modifyPrompt :: (Prompt -> Prompt) -> UzblM ()
modifyPrompt f = modify $ \u -> u{ uzblPrompt = fmap f $ uzblPrompt u }

modifyInput :: ((String,String) -> (String,String)) -> UzblM ()
modifyInput f = modifyPrompt $ \p -> p{ promptInput = f $ promptInput p }

promptInsert :: String -> UzblM ()
promptInsert s = modifyInput $ first (reverse s++)

inputLeft :: Input -> Input
inputLeft (c:il,ir) = (il,c:ir)
inputLeft i = i

inputRight :: Input -> Input
inputRight (il,c:ir) = (c:il,ir)
inputRight i = i

historyPush :: String -> UzblState -> UzblState
historyPush x u = u{ uzblPromptHistory =
    x Seq.<| Seq.take historyLength (uzblPromptHistory u) }

historyUp :: UzblState -> UzblState
historyUp u@UzblState{ uzblPrompt = Just p }
  | n Seq.:< r <- Seq.viewl (uzblPromptHistory u) = 
    u{ uzblPrompt = Just p{ promptInput = unInput n }
     , uzblPromptHistory = if null i then r else r Seq.|> i
     } where i = input (promptInput p)
historyUp u = u

historyDown :: UzblState -> UzblState
historyDown u@UzblState{ uzblPrompt = Just p }
  | r Seq.:> n <- Seq.viewr (uzblPromptHistory u) = 
    u{ uzblPrompt = Just p{ promptInput = unInput n }
     , uzblPromptHistory = if null i then r else i Seq.<| r
     } where i = input (promptInput p)
historyDown u = u

historyFind :: Bool -> UzblState -> UzblState
historyFind dir u@UzblState
  { uzblPrompt = Just p@Prompt{ promptInput = (il,_) }
  , uzblPromptHistory = h } 
  | Just n <- (if dir then Seq.findIndexL else Seq.findIndexR) (reverse il `isPrefixOf`) h
  , (l,i Seq.:< r) <- second Seq.viewl $ Seq.splitAt n h =
    u{ uzblPrompt = Just p{ promptInput = (il,drop (length il) i) }
     , uzblPromptHistory = r Seq.>< l
     }
historyFind _ u = u

promptBinds :: Map.Map ModKey (UzblM ())
promptBinds = Map.fromAscList 
  [ ((0, "BackSpace"),  modifyInput $ first tailSafe)
  , ((0, "Delete"),     modifyInput $ second tailSafe)
  , ((0, "Down"),       modify historyDown)
  , ((0, "End"),        modifyInput $ \i -> (input (swap i),""))
  , ((0, "Escape"),     promptStop)
  , ((0, "Home"),       modifyInput $ (,) "" . input)
  , ((0, "Left"),       modifyInput inputLeft)
  , ((0, "Return"),     promptRun)
  , ((0, "Right"),      modifyInput inputRight)
  , ((0, "Up"),         modify historyUp)
  , ((0, "space"),      promptInsert " ")
  , ((modCtrl, "n"),    modify $ historyFind True)
  , ((modCtrl, "t"),    modify $ historyFind False)
  , ((modCtrl, "u"),    modifyInput $ const ("",""))
  , ((modCtrl, "w"),    modifyInput $ \(il,ir) -> (tailSafe $ dropWhile (not . isSpace) il,ir))
  ]

setPrompt :: String -> UzblM ()
setPrompt = setVar "status_message" . ValStr

promptUpdate :: UzblM ()
promptUpdate =
  maybe (setPrompt "") (\Prompt{ promptPrompt = p, promptInput = (il,ir) } ->
    setPrompt $ "<span bgcolor='#000'>" ++ p ++ "<span face='monospace'>" 
      ++ mlEscape (reverse il) ++ "<span face='sans'>|</span>" ++ mlEscape ir 
      ++ "</span></span>"
    ) . uzblPrompt =<< get

promptBind :: ModKey -> UzblM ()
promptBind mk = do
  bindMap promptBinds (promptInsert . snd) mk
  promptUpdate

promptMode :: Prompt -> UzblM ()
promptMode p = do
  modify $ \u -> u
    { uzblBind = promptBind
    , uzblPrompt = Just p
    }   
  promptUpdate

prompt :: String -> String -> (Maybe String -> UzblM ()) -> UzblM ()
prompt p i e = promptMode $ Prompt
  { promptPrompt = p
  , promptInput = unInput i
  , promptCompletions = Nothing
  , promptExec = e
  }
