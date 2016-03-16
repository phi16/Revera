module Revera.Types where

import Control.Lens

data State = Title | Game | Result

data World = World {
  _yPos :: Float,
  _opTime :: Float,
  _state :: State
}
makeLenses ''World