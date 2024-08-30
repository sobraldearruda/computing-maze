module Maze (Maze (..), cellSize, maze21x21, exampleMaze3) where

import Graphics.Gloss
data Maze = Maze
    { grid :: [[Int]]
    , width :: Int
    , height :: Int
    } deriving (Show, Eq)

scaleFactor :: Float
scaleFactor = 0.7

-- Defina o tamanho da célula do labirinto
cellSize :: Float
cellSize = 40 * scaleFactor

-- Calcule o deslocamento para centralizar o labirinto na tela
calculateMazeOffsetX, calculateMazeOffsetY :: Int -> Float
calculateMazeOffsetX screenWidth = -fromIntegral screenWidth / 2 + 2.5 * cellSize
calculateMazeOffsetY screenHeight = fromIntegral screenHeight / 2 - (1.5 * cellSize)

-- Ajuste a função drawMaze para pintar as paredes de branco e o caminho de preto
drawMaze :: Maze -> Float -> Float -> Picture
drawMaze maze mazeOffsetX mazeOffsetY =
  Translate mazeOffsetX mazeOffsetY (Pictures [drawCell x y cell | (y, row) <- zip [0..] (grid maze), (x, cell) <- zip [0..] row])
  where
    drawCell x y cell
      | cell == 1 = Translate (fromIntegral x * cellSize) (fromIntegral (-y) * cellSize) (color black (rectangleSolid cellSize cellSize))
      | otherwise = Translate (fromIntegral x * cellSize) (fromIntegral (-y) * cellSize) (color white (rectangleSolid cellSize cellSize))
