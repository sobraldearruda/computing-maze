module MazeGame (Pos, Direcao(..), posicaoInicialMazeGame, novaPosicaoMazeGame, desenhaMazeGame, ehCaminhoLivre) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Labirinto (Labirinto, larguraLabirinto, alturaLabirinto, escolherLabirintoAleatorio, desenhaLabirinto)

-- Tipo para representar a posição no labirinto
type Pos = (Int, Int)

posicaoInicialMazeGame :: Pos
posicaoInicialMazeGame = (1, 3)

-- Direções possíveis para o jogador
data Direcao = Cima | Baixo | Esquerda | Direita deriving (Eq, Show)

-- Atualiza a posição do jogador com base na direção e no labirinto
novaPosicaoMazeGame :: Pos -> Direcao -> Labirinto -> Pos
novaPosicaoMazeGame (x, y) dir lab = 
  let novaPos = case dir of
                  Cima -> (x, y - 1)
                  Baixo -> (x, y + 1)
                  Esquerda -> (x - 1, y)
                  Direita -> (x + 1, y)
  in if ehCaminhoLivre novaPos lab then novaPos else (x, y)

-- Verifica se a nova posição é um caminho livre
ehCaminhoLivre :: Pos -> Labirinto -> Bool
ehCaminhoLivre (x, y) lab
  | x < 0 || x >= larguraLabirinto || y < 0 || y >= alturaLabirinto = False
  | otherwise = lab !! y !! x == '.'

-- Desenha o jogador
desenhaMazeGame :: Pos -> Picture
desenhaMazeGame (x, y) = translate (fromIntegral x * tamanhoCelula) 
                                 (-fromIntegral y * tamanhoCelula) 
                                 (color yellow $ circleSolid (tamanhoCelula / 2))

-- Tamanho de cada célula do labirinto
tamanhoCelula :: Float
tamanhoCelula = 20