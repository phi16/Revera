module Revera.Render (render) where

import Graphics.Gloss.Interface.IO.Game
import Control.Monad.Writer.Lazy
import Control.Lens hiding (zoom)

import Revera.Font
import Revera.Types

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
  draw $ color (dark $ dark blue) $ translate (-187) (-100) $ zoom 75 $ polygon [(-1,-1),(1,-1),(-1,1)]
  draw $ color (dark $ dark blue) $ translate (188) (-100) $ zoom 75 $ polygon [(1,-1),(1,1),(-1,1)]
  draw $ color white $ translate 0 (-100) $ pictures [
    zoom 75 $ centerString "R A",
    zoom 25 $ centerString "eVEr"]
  draw $ color white $ translate 0 30 $ zoom 10.6 $ centerString "PREss entER to staRt"
  draw $ color white $ translate (-257) 180 $ zoom 7 $ fontString "find the way to "
  draw $ color white $ translate (-257) 200 $ zoom 7 $ fontString "get out of fRame"
  draw $ color white $ translate 40 120 $ zoom 10 $ fontString $ show $ w^.opTime