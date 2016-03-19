{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module Revera.Field where

import Prelude hiding (LT)
import System.Random
import Control.Monad.State.Lazy
import Control.Lens hiding (indices)
import Data.Traversable
import Data.Array
import Data.Maybe
import qualified Data.Map.Strict as M ((!))
import Data.List
import Graphics.Gloss.Interface.IO.Game

import Revera.Font

data Dir = UD | DD | LD | RD
  deriving (Eq, Show, Enum)
data Tile = UT | DT | LT | RT | UDT | LRT | XT | OT | BT
  deriving (Eq, Show)
newtype Field a = Field (Array (Int,Int) (Tile,a))
  deriving (Eq, Show, Functor)

blankField :: Field ()
blankField = Field $ listArray ((0,0),(0,0)) [(XT,())]

move :: Dir -> (Int,Int) -> (Int,Int)
move UD (x,y) = (x,y-1)
move DD (x,y) = (x,y+1)
move LD (x,y) = (x-1,y)
move RD (x,y) = (x+1,y)

inv :: Dir -> Dir
inv UD = DD
inv DD = UD
inv LD = RD
inv RD = LD

selectDir :: IO [Dir]
selectDir = do
  i3 <- randomRIO (0,3)
  i2 <- randomRIO (0,2)
  i1 <- randomRIO (0,1)
  let f i j = \x -> if x == i then j else if x == j then i else x
  return $ map (toEnum . f i1 1 . f i2 2 . f i3 3) [0..3]

toTile :: Dir -> Tile
toTile UD = UT
toTile DD = DT
toTile LD = LT
toTile RD = RT

unTile :: Tile -> Dir
unTile UT = UD
unTile DT = DD
unTile LT = LD
unTile RT = RD

bidir :: Tile -> Tile
bidir UT = UDT
bidir DT = UDT
bidir LT = LRT
bidir RT = LRT
bidir t = t

toChar :: Tile -> Char
toChar UT = 'K'
toChar DT = 'J'
toChar LT = 'H'
toChar RT = 'L'
toChar UDT = 'T'
toChar LRT = 'S'
toChar OT = 'O'
toChar XT = 'X'
toChar BT = ' '

makeField :: Int -> IO (Field ())
makeField w = Field <$> fmap (fmap $ const ()) <$> let
    w' = (w-1)`div`2
    defArr = array ((-w',-w'),(w',w')) $ [ ((x,y),(BT,Just (x,y))) | x <- [-w'..w'], y <- [-w'..w'] ]
    proc xs f = foldM e False xs where
      e = \case
        False -> f
        True -> const $ return True
    puse = fmap fromJust . preuse
    union p q = do
      e <- find p
      f <- find q
      case f of
        Just f' -> ix f'._2 .= e
        Nothing -> case e of
          Just e' -> ix e'._2 .= Nothing
          Nothing -> return ()
    find Nothing = return Nothing
    find (Just p) = do
      (_,e) <- puse $ ix p
      if e == Just p
        then return e
        else do
          f <- find e
          ix p._2 .= f
          return f
    pretend a = do
      e <- get
      r <- a
      put e
      return r
    outOfBounds (x,y) = x < -w' || w' < x || y < -w' || w' < y
    centerNeighbor = map (flip move (0,0)) [UD .. RD]
  in flip execStateT defArr $ do
    ix (0,0)._1 .= XT
    aDir <- head <$> lift selectDir
    let
      ns = filter (/=move aDir (0,0)) centerNeighbor
      v p' d = case move d p' of
        p
          | outOfBounds p -> return True
          | p`elem`ns -> return False
          | otherwise -> do
            (e,_) <- puse $ ix p
            if e /= BT
              then return False
              else do
                ds <- lift selectDir
                proc ds $ \d' -> do
                  ix p._1 .= toTile d'
                  r <- v p d'
                  if r
                    then union (Just p) Nothing
                    else ix p._1 .= BT
                  return r
      safe a = pretend $ do
        a
        us <- mapM (find . Just) ns
        return $ all (/=Nothing) us
    res <- v (0,0) aDir

    forM_ (indices defArr) $ \p -> do
      (t,_) <- puse $ ix p
      if t == BT
        then do
          c <- lift $ randomIO
          if c < (0.05 :: Float)
            then ix p .= (OT,Just p)
            else do
              ds <- lift $ selectDir
              v <- proc ds $ \d -> case move d p of
                p' -> let q = if outOfBounds p' then Nothing else Just p' in do
                  r <- safe $ union q (Just p)
                  if r
                    then do
                      ix p._1 .= toTile d
                      union q (Just p)
                      return True
                    else return False
              when (not v) $ do
                ix p .= (OT,Just p)
        else return ()

    forM_ (indices defArr) $ \p -> do
      (t,e) <- puse $ ix p
      u <- lift randomIO
      when (u < (0.1 :: Float) && t`notElem`[XT,OT]) $ do
        let
          q = move (inv $ unTile t) p
          q' = if outOfBounds q then Nothing else Just q
        r <- safe $ union (Just p) q'
        t' <- if r
          then do
            union (Just p) q'
            return $ bidir t
          else return t
        ix p._1 .= t'

    forM_ (indices defArr) $ \p -> find $ Just p
    es <- elems <$> get
    let es' = transpose $ map (\x -> take w $ drop x es) [0,w..w*(w-1)]
    -- lift $ putStrLn $ unlines $ map (concat . map (\(x,y) -> show x ++ [head (show y)] ++ " ")) es'
    
    return ()

drawField :: ((Int,Int) -> Tile -> a -> Picture) -> Field a -> Picture
drawField f (Field as) = scale r r $ pictures $ map (\(i,(t,a)) -> f i t a) $ assocs as where
  r = recip $ (+1) $ (2*) $ fromIntegral $ snd $ snd $ bounds as

defDrawF :: (Int,Int) -> Tile -> a -> Picture
defDrawF (fromIntegral -> x,fromIntegral -> y) t a = translate x y $ pictures [
  color (makeColor 0.8 0.8 1 1) $ rectangleWire 1　1,
  color white $ scale 0.4 0.4 $ fontMap M.! toChar t]