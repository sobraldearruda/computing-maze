module Player (Player (..), initialPlayerState, triangulo) where

import Graphics.Gloss
import Maze (cellSize)

-- Criação do tipo de dado Player
data Player = Player
    { playerX     :: Float -- posição x do player
    , playerY     :: Float -- posição y do player
    , playerAngle :: Float -- ângulo/direção do player, para ajustar onde o triângulo aponta
    } deriving (Show, Eq)

-- Estado inicial do player, posição inicial, ângulo inicial 
initialPlayerState :: Float -> Float-> Float -> Player
initialPlayerState mazeOffsetX mazeOffsetY cellSize = Player 
    {
        playerX = mazeOffsetX + cellSize
    ,   playerY = mazeOffsetY - cellSize
    ,   playerAngle = 90
    }

-- Conjunto de pontos para serem passados como parâmetro para desenhar o player
triangulo :: Path
triangulo = [(0, cellSize / 2), (-cellSize / 2,-cellSize / 2), (cellSize / 2,-cellSize / 2)]