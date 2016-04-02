{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}

module Revera.Rank where

import Prelude hiding (LT,GT)
import Control.Lens
import Control.Monad
import qualified Control.Monad.State.Lazy as S hiding (state)
import Graphics.Gloss.Interface.IO.Game
import Data.List
import Data.Maybe
import Data.Array (Array,bounds,listArray)
import System.IO
import System.Directory
import System.Random

import Revera.Field
import Revera.Font

data Rank = Rank {
  _rankings :: [Int],
  _rankIx :: Maybe Int,
  _position :: Float
}
makeLenses ''Rank

initRank :: Rank
initRank = Rank {
  _rankings = [],
  _rankIx = Nothing,
  _position = 0
}

lift = S.lift

type Ranking a = S.StateT Rank IO a

readRank :: Ranking ()
readRank = do
  rs <- lift $ do
    b <- doesFileExist "rank.txt"
    case b of
      True -> do
        h <- openFile "rank.txt" ReadMode
        res <- map read <$> lines <$> hGetContents h
        print res
        hClose h
        return res
      False -> return $ replicate 9 0
  rankings .= rs

saveRank :: Ranking ()
saveRank = do
  rs <- use rankings
  lift $ do
    h <- openFile "rank.txt" WriteMode
    forM_ rs $ \r -> hPrint h r
    hClose h

addRank :: Int -> Ranking ()
addRank d = do
  readRank
  position .= 0
  rs <- use rankings
  let rs' = take 9 $ reverse $ sort $ d:rs
  rankings .= rs'
  rankIx .= elemIndex d rs'
  saveRank
  
stepRank :: Ranking ()
stepRank = do
  p <- use position
  position += 0.01