module Revera.Render (render) where

import Graphics.Gloss.Interface.IO.Game
import Control.Monad.Writer.Lazy
import Control.Lens hiding (zoom)

import Revera.Font
import Revera.Types
import Revera.Field

type Render a = Writer [Picture] a

draw :: Picture -> Render ()
draw = tell . return
onRender :: Render () -> Picture
onRender = simplify . scale 1 (-1) . pictures . execWriter
zoom :: Float -> Picture -> Picture
zoom e = scale e e
onRect :: (Float,Float) -> (Float,Float) -> (Float -> Render ()) -> Render ()
onRect (x,y) (w,h) a = draw $ translate x y $ scale h h $ pictures $ execWriter $ a $ w/h

simplify :: Picture -> Picture
simplify = id

render w = return $ onRender $ do
  let
    z = w^.zooming
    p#q = p*(1-z) + q*z
    f = 1#(8/3)
  onRect (0#(-160*8/3),0#(-130*8/3)) (4/3*f,f) $ \r -> do
    let
      sc = (*5) $ sin $ (*2) $ w^.opTime
      cc = (*5) $ cos $ (*2) $ w^.opTime
    draw $ color (greyN 0.2 * violet) $ translate (-187) (-100+sc) $ zoom 75 $ polygon [(-1,-1),(1,-1),(-1,1)]
    draw $ color (greyN 0.2 * blue) $ translate (188) (-100+sc) $ zoom 75 $ polygon [(1,-1),(1,1),(-1,1)]
    draw $ color white $ translate 0 (-110) $ pictures [
      translate 0 sc $ zoom 75 $ centerString "R A",
      translate 0 (sc/2) $ zoom 25 $ centerString "eVEr"]
    draw $ color white $ translate 0 (5+cc/2) $ zoom 10.6 $ centerString "PREss entER to staRt"
    draw $ color white $ translate (-257) 180 $ zoom 7 $ fontString "find the way to "
    draw $ color white $ translate (-257) 200 $ zoom 7 $ fontString "get out of fRame"
    onRect (160,130) (240,180) $ \r -> do
      when (z<0.99) $ draw $ color white $ rectangleWire r 1
      draw $ translate (-0.18) 0 $ scale 0.75 0.75 $ drawField defDrawF $ w^.curField
      draw $ translate 0.43 0 $ scale 0.36 0.36 $ drawField defDrawF $ w^.curField