module Minotaur (Minotaur (..), initialMinotaurState, moveTowards) where

import Player (Player (..))
import Maze (cellSize)

-- Criação do tipo de dado Minotaur
data Minotaur = Minotaur
    { minotaurX         :: Float -- posição x do minotaur
    , minotaurY         :: Float -- posição y do minotaur
    , minotaurAngle     :: Float -- ângulo do minotaur
    , delayTime :: Float         -- delay da movimentação do minotauro
    } deriving (Show, Eq)

-- Estado inicial do minotauro, posição inicial, ângulo inicial e etc
initialMinotaurState :: Float -> Float-> Float -> Minotaur
initialMinotaurState mazeOffsetX mazeOffsetY _ = Minotaur
    {
        minotaurX = mazeOffsetX
    ,   minotaurY = mazeOffsetY
    ,   minotaurAngle = 90
    ,   delayTime = 1.5
    }

moveTowards :: Minotaur -> Player -> Minotaur
moveTowards minotaur player 
  | dx > dy && x2 > x1 = minotaur { minotaurX = x1 + cellSize
                                  , minotaurY = y1
                                  , minotaurAngle = 90  -- Movendo para a direita
                                  }
  | dx > dy && x2 < x1 = minotaur { minotaurX = x1 - cellSize
                                  , minotaurY = y1
                                  , minotaurAngle = -90 -- Movendo para a esquerda
                                  }
  | dy >= dx && y2 > y1 = minotaur { minotaurX = x1
                                   , minotaurY = y1 + cellSize
                                   , minotaurAngle = 0   -- Movendo para cima
                                   }
  | dy >= dx && y2 < y1 = minotaur { minotaurX = x1
                                   , minotaurY = y1 - cellSize
                                   , minotaurAngle = 180 -- Movendo para baixo
                                   }
  | otherwise = minotaur  -- Caso já esteja na posição correta ou algum caso não previsto
  where
    x1 = minotaurX minotaur 
    y1 = minotaurY minotaur
    x2 = playerX player 
    y2 = playerY player
    dx = abs (x2 - x1)
    dy = abs (y2 - y1)
