module Revera.Render (render) where

import Graphics.Gloss.Interface.IO.Game
import Revera.Font
import Revera.Types
import Control.Monad.Writer.Lazy

type Render a = Writer [Picture] a

draw :: Picture -> Render ()
draw = tell . return
onRender :: Render () -> Picture
onRender = simplify . scale 1 (-1) . pictures . execWriter
zoom :: Float -> Picture -> Picture
zoom e = scale e e

simplify :: Picture -> Picture
simplify = id

render w = return $ onRender $ do
  draw $ color white $ translate 0 (-100) $ pictures [
    zoom 75 $ centerString "R A",
    zoom 25 $ centerString "eVEr"]
  draw $ color white $ translate 0 30 $ zoom 10 $ centerString "prEss any key"