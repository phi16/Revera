module Revera.World where

import Control.Lens
import Revera.Field
import Revera.Game hiding (State)
import Revera.Rank

data State = Title | Game | Result
  deriving (Eq, Show)

data World = World {
  _yPos :: Float,
  _opTime :: Float,
  _state :: State,
  _zooming :: Float,
  _game :: Game,
  _rank :: Rank
}
makeLenses ''World