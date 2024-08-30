module Player (Player (..), initialPlayerState, triangulo) where

import Graphics.Gloss
import Maze (cellSize)

data Player = Player
    { playerX     :: Float
    , playerY     :: Float
    , playerAngle :: Float
    } deriving (Show, Eq)

initialPlayerState :: Float -> Float-> Float -> Player
initialPlayerState mazeOffsetX mazeOffsetY cellSize = Player 
    {
        playerX = mazeOffsetX + cellSize
    ,   playerY = mazeOffsetY - cellSize
    ,   playerAngle = 90
    }

triangulo :: Path
triangulo = [(0, cellSize / 2), (-cellSize / 2,-cellSize / 2), (cellSize / 2,-cellSize / 2)]