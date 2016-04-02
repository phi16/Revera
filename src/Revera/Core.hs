{-# LANGUAGE LambdaCase #-}

module Revera.Core where

import Control.Lens
import Control.Monad.State.Lazy hiding (state)
import System.Random
import Graphics.Gloss.Interface.IO.Game

import Revera.Field
import Revera.World
import Revera.Game hiding (state,State)
import qualified Revera.Game as G (state)
import Revera.Rank hiding (lift)

initialWorld :: World
initialWorld = World {
  _yPos = 0,
  _opTime = 0,
  _state = Title,
  _zooming = 0,
  _game = initGame,
  _rank = initRank
}

handleEvent :: Event -> World -> IO World
handleEvent e w = flip execStateT w $ case e of
  EventKey (SpecialKey KeyEnter) Down _ _ -> case w^.state of
    Title -> do
      state .= Game
      opTime .= 0
      zoom game $ startGame
    Result -> do
      state .= Title
      opTime .= 0
      game .= initGame
      rank .= initRank
    _ -> return ()
  EventKey (SpecialKey KeyEsc) Down _ _ -> do
    state .= Title
    opTime .= 0
    game .= initGame
    rank .= initRank
  EventKey (SpecialKey KeyUp) Down _ _ -> when (w^.state==Game) $ zoom game $ pressKey UD
  EventKey (SpecialKey KeyDown) Down _ _ -> when (w^.state==Game) $ zoom game $ pressKey DD
  EventKey (SpecialKey KeyLeft) Down _ _ -> when (w^.state==Game) $ zoom game $ pressKey LD
  EventKey (SpecialKey KeyRight) Down _ _ -> when (w^.state==Game) $ zoom game $ pressKey RD
  EventKey (SpecialKey KeyUp) Up _ _ -> when (w^.state==Game) $ zoom game $ releaseKey UD
  EventKey (SpecialKey KeyDown) Up _ _ -> when (w^.state==Game) $ zoom game $ releaseKey DD
  EventKey (SpecialKey KeyLeft) Up _ _ -> when (w^.state==Game) $ zoom game $ releaseKey LD
  EventKey (SpecialKey KeyRight) Up _ _ -> when (w^.state==Game) $ zoom game $ releaseKey RD
  _ -> return ()

step :: Float -> World -> IO World
step f w = flip execStateT w $ do
  case w^.state of
    Title  -> zooming += (0 - w^.zooming) / 3
    Game   -> zooming += (1 - w^.zooming) / 3
    Result -> zooming += (6 - w^.zooming) / 3
  when (w^.state == Game) $ do
    when (floor (w^.opTime) /= floor (w^.opTime + f)) $ do
      zoom game $ tickGame
    when (w^.opTime > maxTime) $ do
      state .= Result
      game.speed .= 1
      zoom game $ let
          a = animate >>= \case
            True -> G.state .= Await
            False -> a
        in a
      sc <- use $ game.count
      zoom rank $ addRank sc
  when (w^.state /= Result) $ zoom game $ stepGame
  when (w^.state == Result) $ zoom rank $ stepRank
  opTime += f