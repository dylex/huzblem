module Keys 
  ( Key, Mod, ModKey
  , readModifiers
  , modifierTest
  , modShift, modCtrl, modMod1, modMod4
  , bindMap
  ) where

import Data.Bits
import Data.List
import qualified Data.Map as Map
import Data.Word

import Util

type Key = String
type Mod = Word
type ModKey = (Mod, Key)

modifierList :: [String]
modifierList = [
    "Shift"
  , "ScrollLock"
  , "Ctrl"
  , "Mod1"
  , "Mod2"
  , "Mod3"
  , "Mod4"
  , "Mod5"
  , "Button1"
  , "Button2"
  , "Button3"
  , "Button4"
  , "Button5"
  ]

modifierIndex :: String -> Maybe Int
modifierIndex = (`elemIndex` modifierList)

modifier :: String -> Mod
modifier = maybe 0 bit . modifierIndex

modifiers :: [String] -> Mod
modifiers = foldl' (\i -> maybe i (setBit i) . modifierIndex) 0

readModifiers :: String -> Mod
readModifiers = modifiers . splitOn ('|'==) 

modShift, modCtrl, modMod1, modMod4 :: Mod
modShift = modifier "Shift"
modCtrl = modifier "Ctrl"
modMod1 = modifier "Mod1"
modMod4 = modifier "Mod4"

modifierMask :: Mod
modifierMask = modifiers ["Ctrl","Mod1","Mod4"]

modifierTest :: String -> Mod -> Bool
modifierTest s m = maybe False (testBit m) $ modifierIndex s

mask :: Mod -> ModKey -> ModKey
mask f (m,k) = (m.&.f,k)

bindMap :: Map.Map ModKey a -> (ModKey -> a) -> ModKey -> a
bindMap m d k = maybe (d k) id $ Map.lookup (mask modifierMask k) m
