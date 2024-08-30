module Minotaur (Minotaur (..), initialMinotaurState, moveTowards) where

import Player (Player (..))
import Maze (Maze(..), cellSize, isWalkable)

-- Criação do tipo de dado Minotaur
data Minotaur = Minotaur
    { minotaurX         :: Float -- posição x do minotaur
    , minotaurY         :: Float -- posição y do minotaur
    , minotaurAngle     :: Float -- ângulo do minotaur
    , delayTime :: Float         -- delay da movimentação do minotauro
    } deriving (Show, Eq)

-- Estado inicial do minotauro, posição inicial, ângulo inicial e etc
initialMinotaurState :: Float -> Float-> Float -> Minotaur
initialMinotaurState mazeOffsetX mazeOffsetY cellSize = Minotaur 
    {
        minotaurX = mazeOffsetX + cellSize
    ,   minotaurY = mazeOffsetY - cellSize
    ,   minotaurAngle = 90
    ,   delayTime = 1.0
    }

-- moveTowards :: Minotaur -> Player -> Minotaur
-- moveTowards minotaur player 
--   | dx > dy   = minotaur {minotaurX = x1 + signum (x2 - x1) * cellSize, minotaurY = y1}
--   | otherwise = minotaur {minotaurX = x1, minotaurY = y1 + signum (y2 - y1) * cellSize}
--   where
--     x1 = minotaurX minotaur 
--     y1 = minotaurY minotaur
--     x2 = playerX player 
--     y2 = playerY player
--     dx = abs (x2 - x1)
--     dy = abs (y2 - y1)

-- Função para o minotauro correr atrás do player
-- !! AINDA EM CONSTRUÇÃO !!
moveTowards :: Maze -> Minotaur -> Player -> Minotaur
moveTowards maze minotaur player
  | dx > dy && isWalkable maze (x1 + signum (x2 - x1) * cellSize) y1 =
      minotaur {minotaurX = x1 + signum (x2 - x1) * cellSize, minotaurY = y1, minotaurAngle = if x2 > x1 then 90 else -90}
  | dx <= dy && isWalkable maze x1 (y1 + signum (y2 - y1) * cellSize) =
      minotaur {minotaurX = x1, minotaurY = y1 + signum (y2 - y1) * cellSize, minotaurAngle = if y2 > y1 then 0 else 180}
  | otherwise = minotaur -- Se não pode se mover na direção desejada, fica parado
  where
    x1 = minotaurX minotaur 
    y1 = minotaurY minotaur
    x2 = playerX player 
    y2 = playerY player
    dx = abs (x2 - x1)
    dy = abs (y2 - y1)
