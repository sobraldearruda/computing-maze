module Player (
    Player (..), 
    initialPlayerState, 
    triangulo, 
    hasPlayerWon
) where

import Graphics.Gloss
import Maze (Maze (..), cellSize)

-- Criação do tipo de dado 'Player', que representa o estado do jogador no jogo.
-- O 'Player' possui as seguintes propriedades:
-- 'playerX': Posição x do jogador no labirinto.
-- 'playerY': Posição y do jogador no labirinto.
-- 'playerAngle': Ângulo/direção do jogador, usado para determinar a orientação do triângulo que o representa.
data Player = Player
    { playerX     :: Float
    , playerY     :: Float
    , playerAngle :: Float
    } deriving (Show, Eq)

-- Define o estado inicial do jogador no jogo.
-- Ela recebe as coordenadas iniciais 'mazeOffsetX' e 'mazeOffsetY' do labirinto,
-- e inicializa o jogador em uma posição ligeiramente deslocada dessas coordenadas,
-- com um ângulo inicial de 90 graus.
initialPlayerState :: Float -> Float -> Float -> Player
initialPlayerState mazeOffsetX mazeOffsetY _ = Player
    { playerX = mazeOffsetX + cellSize
    , playerY = mazeOffsetY - cellSize
    , playerAngle = 90
    }

-- Verifica se o jogador alcançou a posição de vitória no labirinto.
-- Ela recebe o estado do labirinto ('maze') e do jogador ('player'), calcula a posição do
-- jogador na grade do labirinto, e verifica se essa posição corresponde à célula de vitória,
-- que é representada pelo valor 2 na grade do labirinto.
hasPlayerWon :: Maze -> Player -> Bool
hasPlayerWon maze player =
    let x = playerX player
        y = playerY player
        gridX = floor ((x + fromIntegral (width maze) * cellSize / 2) / cellSize)
        gridY = floor ((fromIntegral (height maze) * cellSize / 2 - y) / cellSize)
    in grid maze !! gridY !! gridX == 2

-- Conjunto de pontos que define a forma de um triângulo, usado para
-- desenhar o jogador no labirinto. 
-- O triângulo é posicionado com base no tamanho de uma célula do labirinto ('cellSize').
triangulo :: Path
triangulo = [(0, cellSize / 2), (- (cellSize / 2),- (cellSize / 2)), (cellSize / 2,-(cellSize / 2))]
