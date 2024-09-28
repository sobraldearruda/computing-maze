module Minotaur (
    Minotaur (..), 
    initialMinotaurState, 
    moveTowards
) where

import Player (Player (..))
import Maze (cellSize)

-- Criação do tipo de dado 'Minotaur', que representa o estado do minotauro no jogo.
-- O 'Minotaur' possui as seguintes propriedades:
-- 'minotaurX': Posição x do minotauro no labirinto.
-- 'minotaurY': Posição y do minotauro no labirinto.
-- 'minotaurAngle': Ângulo de direção do minotauro (em graus).
-- 'delayTime': Tempo de delay para a movimentação do minotauro.
data Minotaur = Minotaur
    { minotaurX         :: Float
    , minotaurY         :: Float
    , minotaurAngle     :: Float
    , delayTime         :: Float
    } deriving (Show, Eq)

-- Define o estado inicial do minotauro no jogo.
-- Ela recebe as coordenadas iniciais 'mazeOffsetX' e 'mazeOffsetY' do labirinto
-- e inicializa o minotauro nessas posições com um ângulo inicial de 90 graus e
-- um tempo de delay de 1.5 segundos.
initialMinotaurState :: Float -> Float-> Float -> Minotaur
initialMinotaurState mazeOffsetX mazeOffsetY _ = Minotaur
    { minotaurX = mazeOffsetX
    , minotaurY = mazeOffsetY
    , minotaurAngle = 90
    , delayTime = 1.5
    }

-- Movimenta o minotauro na direção do jogador (player).
-- A função recebe o estado atual do minotauro e do jogador e retorna um novo estado
-- do minotauro após a movimentação.
-- O minotauro se move em direção ao jogador com base nas distâncias horizontais (dx) e verticais (dy)
-- entre as posições do minotauro e do jogador:
-- - Se dx > dy e o jogador estiver à direita do minotauro, ele se move para a direita.
-- - Se dx > dy e o jogador estiver à esquerda, ele se move para a esquerda.
-- - Se dy >= dx e o jogador estiver acima do minotauro, ele se move para cima.
-- - Se dy >= dx e o jogador estiver abaixo, ele se move para baixo.
-- Se o minotauro já estiver na posição correta ou houver algum caso não previsto, ele não se move.
moveTowards :: Minotaur -> Player -> Minotaur
moveTowards minotaur player 
  | dx > dy && x2 > x1 = minotaur { minotaurX = x1 + cellSize
                                  , minotaurY = y1
                                  , minotaurAngle = 90  -- Movendo para a direita.
                                  }
  | dx > dy && x2 < x1 = minotaur { minotaurX = x1 - cellSize
                                  , minotaurY = y1
                                  , minotaurAngle = -90 -- Movendo para a esquerda.
                                  }
  | dy >= dx && y2 > y1 = minotaur { minotaurX = x1
                                   , minotaurY = y1 + cellSize
                                   , minotaurAngle = 0  -- Movendo para cima.
                                   }
  | dy >= dx && y2 < y1 = minotaur { minotaurX = x1
                                   , minotaurY = y1 - cellSize
                                   , minotaurAngle = 180 -- Movendo para baixo.
                                   }
  | otherwise = minotaur
  where
    x1 = minotaurX minotaur 
    y1 = minotaurY minotaur
    x2 = playerX player 
    y2 = playerY player
    dx = abs (x2 - x1)
    dy = abs (y2 - y1)
