module Revera.Core where

import Control.Lens
import Control.Monad.State.Lazy hiding (state)
import System.Random
import Graphics.Gloss.Interface.IO.Game

import Revera.Field
import Revera.Types

initialWorld :: World
initialWorld = World {
  _yPos = 0,
  _opTime = 0,
  _state = Title,
  _zooming = 0,
  _curField = blankField
}

handleEvent :: Event -> World -> IO World
handleEvent e w = flip execStateT w $ case e of
  EventKey (SpecialKey KeyEnter) Down _ _ -> do
    state .= if w^.state == Game then Title else Game
    opTime .= 0
  EventKey (SpecialKey KeySpace) Down _ _ -> do
    e <- lift $ randomRIO (1,5)
    ff <- lift $ makeField $ e*2+1
    curField .= ff
  _ -> return ()

step :: Float -> World -> IO World
step f w = flip execStateT w $ do
  if w^.state == Title
    then zooming += (0 - w^.zooming) / 4
    else zooming += (1 - w^.zooming) / 4
  opTime += f