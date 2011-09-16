module Block
  ( Blocks
  , defaultBlocks
  , blockModeDefault, blockModeList, blockModeCurrent
  , loadBlocks
  , saveBlocks
  , BlockMode(..)
  , blockTest
  , blockSet
  ) where

import Control.Monad
import Data.Maybe

import Util
import qualified DomainMap as DM

type Blocks = DM.DomainMap Bool

defaultBlocks :: Blocks
defaultBlocks = DM.empty

data BlockMode 
  = BlockNone
  | BlockUntrusted
  | AllowTrustedCurrent
  | AllowTrusted
  | BlockAll
  deriving (Show, Eq, Enum, Bounded)

blockModeDefault :: BlockMode -> Bool
blockModeDefault BlockNone = True
blockModeDefault BlockUntrusted = True
blockModeDefault _ = False

blockModeList :: BlockMode -> Bool
blockModeList BlockUntrusted = True
blockModeList AllowTrustedCurrent = True
blockModeList AllowTrusted = True
blockModeList _ = False

blockModeCurrent :: BlockMode -> Bool
blockModeCurrent AllowTrustedCurrent = True
blockModeCurrent _ = False

blockTest :: (DM.IsDomain c, DM.IsDomain t) => Blocks -> Maybe c -> BlockMode -> t -> Bool
blockTest bl cur b dom = 
  fromMaybe (blockModeDefault b) $ 
    guard (blockModeList b) >>
      DM.lookupPrefix dom bl'
  where
  bl' 
    | blockModeCurrent b
    , Just cur' <- cur = DM.insert cur' True bl
    | otherwise = bl

blockSet :: DM.IsDomain s => s -> Maybe Bool -> Blocks -> Blocks
blockSet d (Just t) = DM.insert d t
blockSet d Nothing = DM.delete d

trustMark :: String
trustMark = "\tt"

parseBlock :: String -> (String, Bool)
parseBlock = second (trustMark ==) . break (head trustMark ==)

writeBlock :: (String, Bool) -> String
writeBlock (s,t) = s ++ (guard t >> trustMark)

loadBlocks :: FilePath -> IO Blocks
loadBlocks = DM.fromAscList . map parseBlock . lines .=< readFile

saveBlocks :: FilePath -> Blocks -> IO ()
saveBlocks f = writeFile f . unlines . map writeBlock . DM.toAscList
