{-# LANGUAGE TemplateHaskell #-}

module Revera.Game (Game,initGame,stepGame,startGame,drawGame) where

import Control.Lens
import qualified Control.Monad.State.Lazy as S hiding (state)
import Graphics.Gloss.Interface.IO.Game
import System.Random

import Revera.Field
import Revera.Font

data State = Await | Anim | Count Float Int
  deriving (Eq, Show)
data Mode = Auto | Play | Init

data Game = Game {
  _curField :: Field (Float,Bool),
  _nextField :: Field (),
  _state :: State,
  _mode :: Mode,
  _place :: Float
}
makeLenses ''Game

initGame :: Game
initGame = Game {
  _curField = const (0,False) <$> blankField,
  _nextField = blankField,
  _state = Await,
  _mode = Init,
  _place = 0
}

lift = S.lift

stepGame :: Game -> IO Game
stepGame g = flip S.execStateT g $ do
  case g^.mode of
    Init -> do
      u <- lift $ fmap (const (0,False)) <$> makeField 3
      v <- lift $ makeField 3
      curField .= u
      nextField .= v
      mode .= Auto
      place .= 1
    Auto -> case g^.state of
      Await -> do
        d <- lift $ randomRIO (0,3)
        g <- S.get
        S.put =<< lift (inputKey (toEnum d) g)
      _ -> return ()
    Play -> case g^.state of
      Await -> return ()
      _ -> return ()
  place += (0 - g^.place) / 4

inputKey :: Dir -> Game -> IO Game
inputKey d g = flip S.execStateT g $ S.when (g^.state == Await) $ do
  state .= Anim
  curField.attr(0,0)._2 .= True
  curField.attr(0,0)._1 .= 1

startGame :: Game -> IO Game
startGame g = flip S.execStateT g $ do
  -- state .= Count 3 3
  mode .= Play

drawGame :: Game -> Picture
drawGame g = let
    t = g^.place
    p#q = p*(1-t) + q*t
    p##q = p*(1-sqrt t) + q*sqrt t
    dff :: (Int,Int) -> Tile -> (Float,Bool) -> Picture
    dff i t v@(_,False) = defDrawF i t v
    dff i t v@(_,True) = pictures [color (dark blue) $ rectangleSolid 1 1,defDrawF i t v]
  in pictures [
    translate ((-0.18)#0.40) 0 $ scale (0.75##0.36) (0.75##0.36) $ drawField dff $ g^.curField,
    translate (0.43#2.0) 0 $ scale (0.36##0) (0.36##0) $ drawField defDrawF $ g^.nextField]