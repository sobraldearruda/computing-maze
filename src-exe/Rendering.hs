module Rendering (
    GlossState (..),
    drawGlossState,
    glossEventHandler,
    glossTimeHandler,
    initialGlossState,
    fr
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Maze (Maze (..), cellSize, calculateMazeOffsetX, calculateMazeOffsetY, drawMaze, isWalkable)
import Player (Player (..), initialPlayerState, triangulo)
import Minotaur (Minotaur (..), initialMinotaurState, moveTowards)

-- Criação do GlossState, um tipo de dado que vai guardar todo o estado do mundo jogo
data GlossState = GlossState
    { player    :: Player    -- Estado do jogador
    , minotaur  :: Minotaur  -- Estado do minotauro
    , maze      :: Maze
    , time      :: Float  -- Matriz do labirinto
    } deriving (Show, Eq)

fr :: Int
fr = 50

-- Recebe um GlossState e desenha ele na tela
drawGlossState :: GlossState -> Picture
drawGlossState glossState =
  let mazeState = maze glossState
      minotaurState = minotaur glossState
      playerState = player glossState
      screenWidth = (width mazeState + 4) * round cellSize
      screenHeight = (height mazeState + 2) * round cellSize
      mazeOffsetX = calculateMazeOffsetX screenWidth
      mazeOffsetY = calculateMazeOffsetY screenHeight
      minotaur' = Translate (minotaurX minotaurState) (minotaurY minotaurState) (Rotate (minotaurAngle minotaurState) (color blue (polygon triangulo)))
      player' = Translate (playerX playerState) (playerY playerState) (Rotate (playerAngle playerState) (color red (polygon triangulo)))      
  in Pictures [drawMaze mazeState mazeOffsetX mazeOffsetY, player', minotaur']

-- Lida com os eventos e transforma em um novo estado
glossEventHandler :: Event -> GlossState -> GlossState
glossEventHandler (EventKey (SpecialKey KeyUp) Down _ _) glossState =
    let p = player glossState
        mz = maze glossState
        nextX = playerX p
        nextY = playerY p + cellSize
    in if isWalkable mz nextX nextY
       then glossState { player = p { playerX = nextX, playerY = nextY, playerAngle = 0 } }
       else glossState

glossEventHandler (EventKey (SpecialKey KeyDown) Down _ _) glossState =
    let p = player glossState
        mz = maze glossState
        nextX = playerX p
        nextY = playerY p - cellSize
    in if isWalkable mz nextX nextY
       then glossState { player = p { playerX = nextX, playerY = nextY, playerAngle = 180 } }
       else glossState

glossEventHandler (EventKey (SpecialKey KeyLeft) Down _ _) glossState =
    let p = player glossState
        mz = maze glossState
        nextX = playerX p - cellSize
        nextY = playerY p
    in if isWalkable mz nextX nextY
       then glossState { player = p { playerX = nextX, playerY = nextY, playerAngle = -90 } }
       else glossState

glossEventHandler (EventKey (SpecialKey KeyRight) Down _ _) glossState =
    let p = player glossState
        mz = maze glossState
        nextX = playerX p + cellSize
        nextY = playerY p
    in if isWalkable mz nextX nextY
       then glossState { player = p { playerX = nextX, playerY = nextY, playerAngle = 90 } }
       else glossState

glossEventHandler _ glossState = glossState

-- -- Atualiza o estado do Gloss baseado no tempo
-- glossTimeHandler :: Float -> GlossState -> GlossState
-- glossTimeHandler dt glossState =
--   let p = player glossState
--       m = minotaur glossState
--       delay = delayTime m
--       -- Se o atraso tiver passado, mova o minotauro
--       newDelay = delay - dt
--       updatedMinotaur = if newDelay <= 0
--                          then (moveTowards m p) { delayTime = 1.5 }
--                          else m { delayTime = newDelay }
--   in glossState { 
--        player = p { playerX = playerX p, playerY = playerY p, playerAngle = playerAngle p }, 
--        minotaur = updatedMinotaur 
--      }


-- Atualiza o estado do Gloss baseado no tempo
glossTimeHandler :: Float -> GlossState -> GlossState
glossTimeHandler dt glossState =
  let p = player glossState
      m = minotaur glossState
      mazeGrid = maze glossState
      delay = delayTime m
      -- Se o atraso tiver passado, mova o minotauro
      newDelay = delay - dt
      updatedMinotaur = if newDelay <= 0
                         then (moveTowards mazeGrid m p) { delayTime = 1.5 }
                         else m { delayTime = newDelay }
  in glossState { 
       player = p { playerX = playerX p, playerY = playerY p, playerAngle = playerAngle p }, 
       minotaur = updatedMinotaur 
     }


-- Define o estado inicial do jogo
initialGlossState :: Maze -> GlossState
initialGlossState maze =
  let mazeWidth = width maze
      mazeHeight = height maze
      screenWidth = (mazeWidth + 4) * round cellSize
      screenHeight = (mazeHeight + 2) * round cellSize
      mazeOffsetX = calculateMazeOffsetX screenWidth
      mazeOffsetY = calculateMazeOffsetY screenHeight

  in GlossState
      { player = initialPlayerState mazeOffsetX mazeOffsetY cellSize
      , minotaur = initialMinotaurState mazeOffsetX mazeOffsetY cellSize
      , maze = maze
      , time = 0  -- Inicialize o tempo acumulado
      }
