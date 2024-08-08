module Main where

-- import Graphics.Gloss
-- import Graphics.Gloss.Interface.Pure.Game

-- window :: Display
-- window = InWindow "Haskell Snake Game" (640, 480) (100, 100)

-- background :: Color
-- background = white

-- -- Função para imprimir uma matriz
-- printMatrix :: Show a => [[a]] -> IO ()
-- printMatrix = mapM_ printRow
--   where
--     printRow row = putStrLn $ unwords $ map show row

-- -- Exemplo de uso
-- main :: IO ()
-- main = do
--   let matrix = [[1 , 2, 3], [4, 5, 6], [7, 8, 9]]
--   printMatrix matrix

import Graphics.Gloss

-- Define o tamanho de cada célula
cellSize :: Float
cellSize = 20

-- Converte a matriz em uma lista de imagens
matrixToPictures :: [[Int]] -> [Picture]
matrixToPictures matrix = [translate (fromIntegral x * cellSize) (fromIntegral y * cellSize) colorSquare | (y, row) <- zip [0..] matrix, (x, cell) <- zip [0..] row, let colorSquare = if cell == 0 then color black (rectangleSolid cellSize cellSize) else color white (rectangleSolid cellSize cellSize)]

-- Função principal
main :: IO ()
main = do
  let matrix = [[1, 0, 1], [0, 1, 0], [1, 0, 1], [0, 1, 0], [1, 0, 1], [0, 1, 0], [1, 0, 1], [0, 1, 0]]
      picture = pictures (matrixToPictures matrix)
  display (InWindow "Matriz" (300, 300) (100, 100)) black picture

