{-# LANGUAGE MultiWayIf #-}

module Revera.Render (render) where

import Graphics.Gloss.Interface.IO.Game
import Control.Monad.Writer.Lazy
import Control.Lens hiding (zoom)

import Revera.Font
import Revera.World
import Revera.Field
import Revera.Game hiding (state)
import Revera.Rank

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
    onRect (160,130) (240,180) $ \r -> do
      draw $ drawGame (w^.opTime) $ w^.game
      when (z<0.99) $ do
        draw $ color (makeColor 0 0 0.1 1) $ pictures [
          translate (-2-r/2) 0 $ rectangleSolid 4 4,
          translate (2+r/2) 0 $ rectangleSolid 4 4,
          translate 0 (-2.5) $ rectangleSolid 4 4,
          translate 0 2.5 $ rectangleSolid 4 4]
        draw $ color white $ rectangleWire r 1
    draw $ color (greyN 0.2 * violet) $ translate (-187) (-100+sc) $ zoom 75 $ polygon [(-1,-1),(1,-1),(-1,1)]
    draw $ color (greyN 0.2 * blue) $ translate (188) (-100+sc) $ zoom 75 $ polygon [(1,-1),(1,1),(-1,1)]
    draw $ color violet $ translate (-187) (-100+sc) $ zoom 75 $ line [(-1,-1),(1,-1),(-1,1),(-1,-1)]
    draw $ color blue $ translate (188) (-100+sc) $ zoom 75 $ line [(1,-1),(1,1),(-1,1),(1,-1)]
    draw $ color white $ translate 0 (-110) $ pictures [
      translate 0 sc $ zoom 75 $ centerString "R A",
      translate 0 (sc/2) $ zoom 25 $ centerString "eVEr"]
    draw $ color (light cyan) $ translate 0 (5+cc/2) $ zoom 10.6 $ centerString "PREss entER to staRt"
    draw $ color white $ translate (-257) 180 $ zoom 7 $ fontString "find the way to "
    draw $ color white $ translate (-257) 200 $ zoom 7 $ fontString "get out of fRame"
    draw $ color (makeColor 0.8 0.8 1 1) $ translate (-255) 100 $ zoom 30 $ fontString "HJKL"
  when (w^.state == Result) $ do
    let
      scr = w^.game.count
      ix = w^.rank.rankIx
      pos = w^.rank.position
      ani = 1-exp(-pos*50+1)
      dif x = ani*x + (1-ani)*500
      (sz,condStr) = if
        | scr == 0 -> (74,"null")
        | scr < 10 -> (74,"PooR")
        | scr < 30 -> (37,"bEginneR")
        | scr < 50 -> (74,"good")
        | scr < 60 -> (74,"nIce")
        | scr < 70 -> (45,"PeRfEct")
        | scr < 100 -> (35,"ExcellEnt")
        | otherwise -> (31,"incrEdIble")
      t i j = translate i j $ rotate (30*ani) $ zoom sz $ centerString condStr
    draw $ color (withAlpha 0.07 orange) $ pictures [ t i j | i<-[-2..2], j<-[-2..2] ]
    forM_ (zip [0..] $ w^.rank.rankings) $ \(i,r) -> do
      let
        c = if
          | r < 50 -> greyN 0.5
          | r < 60 -> makeColor 0.7 0.5 0.2 1
          | r < 70 -> greyN 0.8
          | r < 100 -> yellow
          | otherwise -> makeColor 0.5 1 1 1
        x = -250
        y = fromIntegral i * 29 - 40
        str = show (i+1) ++ ". " ++ show r
      draw $ color c $ translate x (dif y) $ zoom 10 $ fontString str
    draw $ color cyan $ translate (-50) (dif 160) $ zoom 10 $ fontString "PREss entER to"
    draw $ color cyan $ translate (-50) (dif 190) $ zoom 10 $ fontString "back to title"