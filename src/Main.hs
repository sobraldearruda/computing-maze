module Main where

import Graphics.Gloss
import Maze (Maze (..), cellSize, mazes)
import Rendering (drawGlossState, glossEventHandler, glossTimeHandler, initialGlossState, fr)
import AppRendering (AppState(..), drawAppState, appEventHandler, appTimeHandler)
import Menu (MenuState(..))

-- Função principal do jogo.
-- Configura a janela do jogo, define o estado inicial e inicia o loop principal do jogo.
main :: IO()
main = do
  let screenWidth = 900
      screenHeight = 600
      dm = InWindow "Computing Maze" (screenWidth, screenHeight) (100, 60)
      initialState = AppState MainMenu Nothing
  play dm
    black
    60
    initialState
    drawAppState
    appEventHandler
    appTimeHandler
