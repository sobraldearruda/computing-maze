module Rendering (
    drawGlossState,
    glossEventHandler,
    glossTimeHandler,
    initialGlossState
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Maze (Maze (..))
import Player (Player (..))
import Minotaur (Minotaur (..))

data GlossState = GlossState
    { player    :: Player    -- Estado do jogador
    , minotaur  :: Minotaur  -- Estado do minotauro
    , maze      :: Maze
    , time      :: Float  -- Matriz do labirinto
    } deriving (Show, Eq)

fr :: Int
fr = 50

drawGlossState :: GlossState -> Picture
drawGlossState glossState =
  let mazeState = maze glossState
      minotaurState = minotaur glossState
      playerState = player glossState
      cellSize' = cellSize mazeState
      screenWidth = (width mazeState + 4) * round cellSize'
      screenHeight = (height mazeState + 2) * round cellSize'
      mazeOffsetX = calculateMazeOffsetX screenWidth
      mazeOffsetY = calculateMazeOffsetY screenHeight
      minotaur' = Translate (minotaurX minotaurState) (minotaurY minotaurState) (Rotate (minotaurAngle minotaurState) (color blue (rectangleSolid cellSize' cellSize')))
      player' = Translate (playerX playerState) (playerY playerState) (Rotate (playerAngle playerState) (color red (rectangleSolid cellSize' cellSize'))) -- Cor da peça do jogador ajustada para vermelho, por exemplo
  in Pictures [drawMaze mazeState mazeOffsetX mazeOffsetY, player', minotaur']

glossEventHandler :: Event -> GlossState -> GlossState
glossEventHandler (EventKey (SpecialKey KeyUp) Down _ _) glossState =
    let p = player glossState
        cellSize' = cellSize (mazeState glossState)
        updatedPlayer = p { playerX = playerX p, playerY = playerY p + cellSize', playerAngle = 0 }
    in glossState { player = updatedPlayer }

glossEventHandler (EventKey (SpecialKey KeyDown) Down _ _) glossState =
    let p = player glossState
        cellSize' = cellSize (mazeState glossState)
        updatedPlayer = p { playerX = playerX p, playerY = playerY p - cellSize', playerAngle = -180 }
    in glossState { player = updatedPlayer }

glossEventHandler (EventKey (SpecialKey KeyLeft) Down _ _) glossState =
    let p = player glossState
        cellSize' = cellSize (mazeState glossState)
        updatedPlayer = p { playerX = playerX p - cellSize', playerY = playerY p, playerAngle = -90 }
    in glossState { player = updatedPlayer }

glossEventHandler (EventKey (SpecialKey KeyRight) Down _ _) glossState =
    let p = player glossState
        cellSize' = cellSize (mazeState glossState)
        updatedPlayer = p { playerX = playerX p + cellSize', playerY = playerY p, playerAngle = 90 }
    in glossState { player = updatedPlayer }

glossEventHandler _ glossState = glossState

-- Atualiza o estado do Gloss baseado no tempo
glossTimeHandler :: Float -> GlossState -> GlossState
glossTimeHandler dt glossState =
  let p = player glossState
      m = minotaur glossState
      delay = delayTime m
      -- Se o atraso tiver passado, mova o minotauro
      newDelay = delay - dt
      updatedMinotaur = if newDelay <= 0
                         then (moveTowards m p) { delayTime = 1.5 }
                         else m { delayTime = newDelay }
  in glossState { 
       player = p { playerX = playerX p, playerY = playerY p, playerAngle = playerAngle p }, 
       minotaur = updatedMinotaur 
     }