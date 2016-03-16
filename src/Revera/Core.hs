module Revera.Core where

import Graphics.Gloss.Interface.IO.Game

initialWorld = 0

render w = return $ color white $ circle 10

handleEvent e w = return w

step f w = return w