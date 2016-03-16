module Revera.Font (fontMap,fontString,centerString) where 

import Data.Maybe
import Data.Map.Strict hiding (lookup,map)
import Graphics.Gloss.Interface.IO.Game

fontString :: String -> Picture
fontString xs = pictures $ zipWith (\x i -> translate (2.5*i) 0 $ fontMap ! x) xs [0..]

centerString :: String -> Picture
centerString xs = pictures $ zipWith (\x i -> translate (2.5*i - w) 0 $ fontMap ! x) xs [0..] where
  w = measureString xs / 2

measureString :: String -> Float
measureString [] = 0
measureString xs = (fromIntegral (length xs) - 1) * 2.5

fontMap :: Map Char Picture
fontMap = insert 'O' (circle 0.3) $ fmap toPic $ fromList fs where
  toPic :: [[Float]] -> Picture
  toPic vs = scale (1/2) (1/2) $ translate (-2) (-2) $ pictures $ map toPic' vs
  toPic' :: [Float] -> Picture
  toPic' vs = line $ toPath vs
  toPath :: [Float] -> Path
  toPath [] = []
  toPath (x:y:xs) = (x,y) : toPath xs

fs :: [(Char, [[Float]])]
fs = [
  ('a',[[0,0,4,0,4,4],[4,2,2,2,0,4]]),
  ('b',[[0,0,0,4,4,4,2,2,0,2]]),
  ('c',[[4,0,0,0,0,2,2,4,4,4]]),
  ('d',[[4,0,4,4,0,4,2,2,4,2]]),
  ('e',[[4,0,2,0,0,2,2,4,4,4],[0,2,4,2]]),
  ('f',[[4,0,2,0,2,4,0,4],[2,2,0,2]]),
  ('g',[[2,2,0,2,0,0,4,0,0,4]]),
  ('h',[[0,0,0,4],[0,2,4,0,4,4]]),
  ('i',[[2,0,2,4],[0,4,4,4]]),
  ('j',[[2,0,4,0,4,4,2,4,0,2]]),
  ('k',[[0,0,0,4,4,0],[2,2,4,4]]),
  ('l',[[0,0,2,0,2,4,4,4]]),
  ('m',[[0,4,0,2,4,0,4,4],[2,1,2,4]]),
  ('n',[[0,4,0,0,4,2,4,4]]),
  ('o',[[2,0,4,0,4,4,0,4,0,2,2,0]]),
  ('p',[[0,4,0,0,4,0,0,3]]),
  ('q',[[4,4,4,0,0,0,4,3]]),
  ('r',[[0,4,0,0,4,0,0,3],[2,1.5,4,4]]),
  ('s',[[4,0,2,0,4,4,0,4,0,2]]),
  ('t',[[0,0,4,0],[2,0,2,4]]),
  ('u',[[0,0,0,4,4,2],[4,0,4,4]]),
  ('v',[[0,0,0,2,4,4,4,0]]),
  ('w',[[0,0,0,4,4,2,4,0],[2,0,2,3]]),
  ('x',[[0,0,4,4],[4,0,0,4]]),
  ('y',[[0,0,2,2,4,2],[4,0,4,4]]),
  ('z',[[0,2,0,0,4,0,2,4,4,4]]),
  ('A',[[0,0,4,0,4,4,0,4,4,0]]),
  ('B',[[0,0,4,0,0,4,4,4,0,0]]),
  ('E',[[0,0,2,0,4,2,2,4,0,4],[0,2,4,2]]),
  ('I',[[0,0,4,0],[2,0,2,4],[0,4,4,4]]),
  ('Q',[[0,0,4,0,4,4,0,0]]),
  ('R',[[0,0,4,0,0,4,0,0],[2,2,4,4]]),
  ('V',[[0,0,2,4,4,0]]),

  ('0',fromJust $ lookup 'o' fs),
  ('1',fromJust $ lookup 'I' fs),
  ('2',fromJust $ lookup 'z' fs),
  ('3',fromJust $ lookup 'E' fs),
  ('4',fromJust $ lookup 'y' fs),
  ('5',fromJust $ lookup 's' fs),
  ('6',fromJust $ lookup 'b' fs),
  ('7',fromJust $ lookup 'Q' fs),
  ('8',fromJust $ lookup 'B' fs),
  ('9',fromJust $ lookup 'g' fs),
  (' ',[]),

  ('H',[[1,1,0,2,1,3],[0,2,4,2]]),
  ('J',[[1,3,2,4,3,3],[2,0,2,4]]),
  ('K',[[3,1,2,0,1,1],[2,0,2,4]]),
  ('L',[[3,3,4,2,3,1],[0,2,4,2]]),
  ('S',[[1,1,0,2,1,3],[3,3,4,2,3,1],[0,2,4,2]]),
  ('T',[[3,1,2,0,1,1],[1,3,2,4,3,3],[2,0,2,4]]),
  ('X',[[2,0.5,0.5,2,2,3.5,3.5,2,2,0.5]])]