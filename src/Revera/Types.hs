module Revera.Types where

import Control.Lens
import Revera.Field

data State = Title | Game | Result
  deriving (Eq, Show)

data World = World {
  _yPos :: Float,
  _opTime :: Float,
  _state :: State,
  _zooming :: Float,
  _curField :: Field ()
}
makeLenses ''World