module Prompt
  ( promptMode
  , promptBind
  , completerSet
  ) where

import Data.Char
import Data.List
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
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
  p <- gets uzblBindings
  promptExec p Nothing

promptRun :: UzblM ()
promptRun = do
  u@UzblState{ uzblBindings = p } <- get
  let s = input (promptInput p)
  put $ historyPush s u
  promptExec p $ Just s

modifyPrompt :: (Bindings -> Bindings) -> UzblM ()
modifyPrompt = modifyBindings

modifyInput :: ((String,String) -> (String,String)) -> UzblM ()
modifyInput f = modifyPrompt $ \p -> p{ promptInput = f $ promptInput p }

promptInsert :: String -> UzblM ()
promptInsert s = modifyInput $ first (reverse s++)

pasteInput :: UzblM ()
pasteInput = io paste >>= maybe nop promptInsert

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
historyUp u@UzblState{ uzblBindings = p }
  | n Seq.:< r <- Seq.viewl (uzblPromptHistory u) = 
    u{ uzblBindings = p{ promptInput = unInput n }
     , uzblPromptHistory = if null i then r else r Seq.|> i
     } where i = input (promptInput p)
historyUp u = u

historyDown :: UzblState -> UzblState
historyDown u@UzblState{ uzblBindings = p }
  | r Seq.:> n <- Seq.viewr (uzblPromptHistory u) = 
    u{ uzblBindings = p{ promptInput = unInput n }
     , uzblPromptHistory = if null i then r else i Seq.<| r
     } where i = input (promptInput p)
historyDown u = u

historyFind :: Bool -> UzblState -> UzblState
historyFind dir u@UzblState
  { uzblBindings = p@Prompt{ promptInput = (il,_) }
  , uzblPromptHistory = h } 
  | Just n <- (if dir then Seq.findIndexL else Seq.findIndexR) (reverse il `isPrefixOf`) h
  , (l,i Seq.:< r) <- second Seq.viewl $ Seq.splitAt n h =
    u{ uzblBindings = p{ promptInput = (il,drop (length il) i) }
     , uzblPromptHistory = r Seq.>< l
     }
historyFind _ u = u

complete :: UzblM ()
complete = do
  u@UzblState{ uzblBindings = p } <- get
  maybe nop (\c ->
      put u{ uzblBindings = p{ promptInput = unInput c } }) 
    =<< promptCompleter p (input (promptInput p))

promptBinds :: Map.Map ModKey (UzblM ())
promptBinds = Map.fromAscList 
  [ ((0, "BackSpace"),  modifyInput $ first tailSafe)
  , ((0, "Delete"),     modifyInput $ second tailSafe)
  , ((0, "Down"),       modify historyDown)
  , ((0, "End"),        modifyInput $ \(a,b) -> (input (b,a),""))
  , ((0, "Escape"),     promptStop)
  , ((0, "Home"),       modifyInput $ (,) "" . input)
  , ((0, "Left"),       modifyInput inputLeft)
  , ((0, "Return"),     promptRun)
  , ((0, "Right"),      modifyInput inputRight)
  , ((0, "Tab"),        complete)
  , ((0, "Up"),         modify historyUp)
  , ((0, "space"),      promptInsert " ")
  , ((modShift, "Insert"), pasteInput)
  , ((modCtrl, "n"),    modify $ historyFind True)
  , ((modCtrl, "t"),    modify $ historyFind False)
  , ((modCtrl, "u"),    modifyInput $ const ("",""))
  , ((modCtrl, "w"),    modifyInput $ \(il,ir) -> (tailSafe $ dropWhile (not . isSpace) il,ir))
  ]

setPrompt :: String -> UzblM ()
setPrompt = setVar "status_message" . ValStr

promptUpdate :: UzblM ()
promptUpdate = pu =<< gets uzblBindings where
  pu Prompt{ promptPrompt = p, promptInput = (il,ir) } =
    setPrompt $ "<span bgcolor='#000' color='#CCC'>" ++ p ++ "<span face='monospace'>" 
      ++ mlEscape (reverse il) ++ "<span face='sans'>|</span>" ++ mlEscape ir 
      ++ "</span></span>"
  pu _ = setPrompt ""

promptBind :: ModKey -> UzblM ()
promptBind mk = do
  bindMap promptBinds (promptInsert . snd) mk
  promptUpdate

promptMode :: String -> String -> Completer -> (Maybe String -> UzblM ()) -> UzblM ()
promptMode p i c e = do
  modifyBindings $ const Prompt
    { promptPrompt = p
    , promptInput = unInput i
    , promptCompleter = c
    , promptExec = e
    }
  promptUpdate

completerSet :: Set.Set String -> String -> Maybe String
completerSet s x
  | e = Just x
  | Just (y,_) <- Set.minView r
  , x `isPrefixOf` y = Just y
  | otherwise = Nothing
  where
    (_,e,r) = Set.splitMember x s
