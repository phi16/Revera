module Revera.Core where

import Control.Lens
import Control.Monad.State.Lazy hiding (state)
import System.Random
import Graphics.Gloss.Interface.IO.Game

import Revera.Field
import Revera.Types
import Revera.Game

initialWorld :: World
initialWorld = World {
  _yPos = 0,
  _opTime = 0,
  _state = Title,
  _zooming = 0,
  _game = initGame
}

handleEvent :: Event -> World -> IO World
handleEvent e w = flip execStateT w $ case e of
  EventKey (SpecialKey KeyEnter) Down _ _ -> case w^.state of
    Title -> do
      state .= Game
      g' <- lift $ startGame $ w^.game
      game .= g'
    _ -> return ()
  _ -> return ()

step :: Float -> World -> IO World
step f w = flip execStateT w $ do
  if w^.state == Title
    then zooming += (0 - w^.zooming) / 3
    else zooming += (1 - w^.zooming) / 3
  g' <- lift $ stepGame $ w^.game
  game .= g'
  opTime += f