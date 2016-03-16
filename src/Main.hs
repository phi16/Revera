import Graphics.Gloss.Interface.IO.Game
import Revera.Core

main = playIO d c i w r h s where
  d = InWindow "Revera" (640,480) (300,300)
  c = makeColor 0 0 0.1 1.0
  i = 60
  w = initialWorld
  r = render
  h = handleEvent
  s = step