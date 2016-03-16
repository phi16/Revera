module Revera.Core where

import Control.Lens

import Revera.Font
import Revera.Types

initialWorld = World {
  _yPos = 0,
  _opTime = 0,
  _state = Title
}

handleEvent e w = return w

step f w = do
  return $ w {
    _opTime = w^.opTime + f
  }