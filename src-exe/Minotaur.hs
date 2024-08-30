module Minotaur (Minotaur (..), initialMinotaurState, moveTowards) where

import Player (Player (..))
import Maze (cellSize)

data Minotaur = Minotaur
    { minotaurX         :: Float
    , minotaurY         :: Float
    , minotaurAngle     :: Float
    , delayTime :: Float
    } deriving (Show, Eq)

initialMinotaurState :: Float -> Float-> Float -> Minotaur
initialMinotaurState mazeOffsetX mazeOffsetY cellSize = Minotaur 
    {
        minotaurX = mazeOffsetX - cellSize
    ,   minotaurY = mazeOffsetY - cellSize
    ,   minotaurAngle = 90
    ,   delayTime = 1.5
    }

moveTowards :: Minotaur -> Player -> Minotaur
moveTowards minotaur player 
  | dx > dy   = minotaur {minotaurX = x1 + signum (x2 - x1) * cellSize, minotaurY = y1}
  | otherwise = minotaur {minotaurX = x1, minotaurY = y1 + signum (y2 - y1) * cellSize}
  where
    x1 = minotaurX minotaur 
    y1 = minotaurY minotaur
    x2 = playerX player 
    y2 = playerY player
    dx = abs (x2 - x1)
    dy = abs (y2 - y1)