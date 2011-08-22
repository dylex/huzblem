module Readline
  (
  ) where

import Util
import Uzbl

abort :: UzblM ()
abort = do


inputBinds :: Map.Map ModKey (UzblM ())
inputBinds = Map.fromAscList
  [ ((0, "Escape"),   defa
