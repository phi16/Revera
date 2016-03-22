{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}

module Revera.Game (Game,initGame,stepGame,startGame,tickGame,drawGame,pressKey,releaseKey) where

import Prelude hiding (LT,GT)
import Control.Lens
import qualified Control.Monad.State.Lazy as S hiding (state)
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.Maybe
import Data.Array (Array,bounds,listArray)

import Revera.Field
import Revera.Font

data State = Await | Anim | Count Int
  deriving (Eq, Show)
data Mode = Auto | Play | Init
  deriving (Eq, Show)
data Anim = Animate | Freeze
  deriving (Eq, Show)

data Game = Game {
  _curField :: Field (Float,Maybe (Tile,Anim)),
  _nextField :: Field (),
  _glowing :: Maybe (Float,Bool),
  _state :: State,
  _mode :: Mode,
  _place :: Float,
  _speed :: Float,
  _count :: Int,
  _vibra :: Float,
  _rand :: Float,
  _keys :: Array Dir Bool
}
makeLenses ''Game

initGame :: Game
initGame = Game {
  _curField = const (0,Nothing) <$> blankField,
  _nextField = blankField,
  _glowing = Nothing,
  _state = Await,
  _mode = Init,
  _place = 0,
  _speed = 0.1,
  _count = 0,
  _vibra = 0,
  _rand = 0,
  _keys = listArray (UD,RD) $ repeat False
}

lift = S.lift
when = S.when

type Gaming a = S.StateT Game IO a

stepGame :: Gaming ()
stepGame = do
  m <- use mode
  s <- use state
  case m of
    Init -> do
      u <- lift $ fmap (const (0,Nothing)) <$> makeField 3
      v <- lift $ makeField 3
      curField .= u
      nextField .= v
      mode .= Auto
      place .= 1
    Auto -> case s of
      Await -> do
        d <- lift $ randomRIO (0,3)
        inputKey (toEnum d)
      Anim -> do
        a <- animate
        if a
          then genField
          else return ()
      _ -> return ()
    Play -> case s of
      Await -> do
        k <- preuse $ keys . ifolded . filtered id . asIndex
        case k of
          Nothing -> return ()
          Just d -> inputKey d
      Anim -> do
        a <- animate
        if a
          then genField
          else return ()
      _ -> return ()
  p <- use place
  place += (0 - p) / 1.1
  vi <- use vibra
  vibra .= if vi < 0.1 then 0 else vi-0.1
  rn <- lift randomIO
  rand .= rn

animate :: Gaming Bool
animate = do
  s <- use speed
  f <- use $ curField.att
  f' <- ifor f $ \(x,y) a@(t,(p,m)) -> return $ case m of
    Nothing -> let
        es = mapMaybe (\d' -> f ^? itraversed . withIndex . index (move d' (x,y))) [UD .. RD]
        fu ((i,j),(OT,(p,Just(_,_)))) = False
        fu ((i,j),(_,(p,Just(t',_)))) = p > 0.8 && (x,y)`elem`tileMove t' (i,j)
        fu _ = False
      in if any fu es
        then ((t,(0,Just (t,Animate))),(True,True))
        else (a,(False,False))
    Just (t',Animate) -> (,(True,False)) $ if p + s < 1
      then (t,(p+s,m))
      else (t,(1,Just (t',Freeze)))
    Just (t',Freeze) -> (a,(False,False))
  curField.att .= (fst <$> f')
  won <- return $ flip iany f $ \(x,y) (_,(_,m)) -> case m of
    Just (t',Freeze) -> any (\i -> f ^? ix i == Nothing) $ tileMove t' (x,y)
    _ -> False
  g <- use glowing
  let
    (gDone,g') = case g of
      Nothing -> (False,Nothing)
      Just (x,b) -> (x > 0.999,Just (x + 0.05,b))
  glowing .= g'
  when (g == Nothing && none (fst.snd) f') $ do
    m <- use mode 
    when won win
    glowing .= Just (0,won)
    when (not won) $ vibra .= 3
  when (any (snd.snd) f') $ vibra .= 1.5
  return gDone

calcLevel :: Int -> Int
calcLevel d
  | d < 10 = 3
  | d < 30 = 5
  | d < 50 = 7
  | d < 100 = 9
  | otherwise = 11

win :: Gaming ()
win = do
  d <- use count
  count += 1
  m <- use mode
  when (m == Play) $ speed .= if
    | d < 10 -> 0.1
    | d < 30 -> 0.15
    | d < 50 -> 0.2
    | d < 100 -> 0.3
    | otherwise -> 0.5

isNum :: Field a -> Bool
isNum (Field a) = uncurry (/=) $ snd $ bounds a

encodeNum :: Int -> Field ()
encodeNum n = Field $ listArray ((-1,-1),(n,0)) $ repeat (XT,())

decodeNum :: Field a -> Int
decodeNum (Field a) = fst $ snd $ bounds a

genField :: Gaming ()
genField = do
  lv <- calcLevel <$> use count
  m <- use mode
  nf <- use nextField
  curField .= (const (0,Nothing) <$> nf)
  v <- lift $ makeField $ if m == Play then lv else 3
  nextField .= v
  glowing .= Nothing
  place .= 1
  state .= Await

inputKey :: Dir -> Gaming ()
inputKey d = do
  s <- use state
  when (s == Await) $ do
    state .= Anim
    curField.attr(0,0)._2 .= Just (toTile d,Animate)
    curField.attr(0,0)._1 .= 0
    vibra .= 2

startGame :: Gaming ()
startGame = do
  speed .= 0.1
  count .= 0
  state .= Count 2
  glowing .= Nothing
  nf <- use nextField
  curField .= (const (0,Nothing) <$> encodeNum 3)
  nextField .= encodeNum 2
  mode .= Play
  return ()

tickGame :: Gaming ()
tickGame = do
  s <- use state
  case s of
    Count 0 -> genField
    Count n -> do
      nf <- use nextField
      curField .= (const (0,Nothing) <$> nf)
      if n == 1
        then do
          v <- lift $ makeField 3
          nextField .= v
        else nextField .= encodeNum (n-1)
      state .= Count (n-1)
      place .= 1
    _ -> return ()

drawGame :: Float -> Game -> Picture
drawGame curT' g = let
    maxT = 103
    curT = maxT - curT'
    t = g^.place
    p#q = p*(1-t) + q*t
    p##q = p*(1-sqrt t) + q*sqrt t
    dff :: (Int,Int) -> Tile -> (Float,Maybe (Tile,Anim)) -> Picture
    dff i t v@(_,Nothing) = defDrawF i t v
    dff i t v@(d,Just (dt,_)) = let
        u = sqrt d
        (xu,yu,wu,hu) = case dt of
          UT -> (0,0.5-u*0.5,1,u)
          DT -> (0,-0.5+u*0.5,1,u)
          LT -> (0.5-u*0.5,0,u,1)
          RT -> (-0.5+u*0.5,0,u,1)
          UDT -> (0,0,1,u)
          LRT -> (0,0,u,1)
          OT -> (0,0,u,u)
      in pictures [
        color (makeColor 0 0.3 0.7 1) $ translate xu yu $ rectangleSolid wu hu,
        color cyan $ translate xu yu $ rectangleWire wu hu,
        defDrawF i t v]
    frame :: Float -> Color -> Picture
    frame d c = pictures $ flip map [0..3] $ \i -> let
        w = 1 + min 1 d*0.1 - i*0.03
        cu = c * greyN ((i+1) / 4)
      in color cu $ rectangleSolid w w
    glows :: Picture
    glows = case g^.glowing of
      Nothing -> blank
      Just (d,True) -> frame (sqrt $ d*2) yellow
      Just (d,False) -> frame (sqrt $ d*2) red
    background :: Picture
    background = color (makeColor 0 0 0.1 1) $ rectangleSolid 1 1
    baseCol = makeColor 0.7 0.7 1 1
    frameCol = makeColor 0.8 0.8 1 1
    vaniCol = makeColor 0.5 0.5 1 1
    rrx = (/300) $ (g^.vibra*) $ cos $ g^.rand * 2 * pi
    rry = (/300) $ (g^.vibra*) $ sin $ g^.rand * 2 * pi
    drawF fu f = if isNum f
      then pictures [
        scale 0.2 0.2 $ color (light cyan) $ fontString $ show $ decodeNum f,
        color white $ rectangleWire 1 1]
      else drawField fu f
    restTime = if 0 < curT && curT < maxT-3 && g^.mode == Play
      then let
          str = take 4 (show curT) ++ drop 2 (show $ g^.rand)
          c = if curT < 10 then magenta else vaniCol
        in color c $ fontString str
      else blank
  in pictures [
    translate rrx rry $ translate ((-0.18)#0.40) 0 $ scale (0.75##0.36) (0.75##0.36) $ pictures [
      glows,
      background,
      drawF dff $ g^.curField],
    translate (0.43#2.0) 0 $ scale (0.36##0) (0.36##0) $ drawF defDrawF $ g^.nextField,
    translate 0.43 (-0.25) $ scale 0.03 0.03 $ color baseCol $ centerString "next",
    translate 0.3 0.25 $ scale 0.03 0.03 $ color baseCol $ fontString $ show $ g^.count,
    translate 0.28 0.35 $ scale 0.016 0.016 $ restTime]

pressKey :: Dir -> Gaming ()
pressKey d = keys.ix d .= True

releaseKey :: Dir -> Gaming ()
releaseKey d = keys.ix d .= False