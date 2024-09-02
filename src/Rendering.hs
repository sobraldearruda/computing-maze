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
import Maze (Maze (..), cellSize, calculateMazeOffsetX, calculateMazeOffsetY, drawMaze, isWalkable, mazes)
import Player (Player (..), initialPlayerState, triangulo, hasPlayerWon)
import Minotaur (Minotaur (..), initialMinotaurState, moveTowards)
import Enigmas (EnigmaState(..), initialEnigmaState, renderEnigma, enigmaEventHandler)

data GameState = Playing | SolvingEnigma | GameOver | Won deriving (Eq, Show)

data GlossState = GlossState
  { player :: Player
  , minotaur :: Minotaur
  , maze :: Maze
  , mazeIndex :: Int
  , time :: Int
  , gameState :: GameState
  , enigmaState :: EnigmaState
  } deriving (Eq, Show)

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
  in case gameState glossState of
    Playing -> Pictures [drawMaze mazeState mazeOffsetX mazeOffsetY, player', minotaur']
    SolvingEnigma -> renderEnigma (enigmaState glossState)
    GameOver -> Translate (-200) 100 $ Color red $ Scale 0.5 0.5 $ Text "Game Over"
    Won -> Translate (-200) 100 $ Color green $ Scale 0.5 0.5 $ Text "You Won!"

-- Lida com os eventos e transforma em um novo estado
glossEventHandler :: Event -> GlossState -> GlossState
glossEventHandler event glossState =
  case gameState glossState of
    Playing ->
      case event of
        (EventKey (SpecialKey KeyUp) Down _ _) ->
          let p = player glossState
              mz = maze glossState
              nextX = playerX p
              nextY = playerY p + cellSize
          in if isWalkable mz nextX nextY
             then glossState { player = p { playerX = nextX, playerY = nextY, playerAngle = 0 } }
             else glossState

        (EventKey (SpecialKey KeyDown) Down _ _) ->
          let p = player glossState
              mz = maze glossState
              nextX = playerX p
              nextY = playerY p - cellSize
          in if isWalkable mz nextX nextY
             then glossState { player = p { playerX = nextX, playerY = nextY, playerAngle = 180 } }
             else glossState

        (EventKey (SpecialKey KeyLeft) Down _ _) ->
          let p = player glossState
              mz = maze glossState
              nextX = playerX p - cellSize
              nextY = playerY p
          in if isWalkable mz nextX nextY
             then glossState { player = p { playerX = nextX, playerY = nextY, playerAngle = -90 } }
             else glossState

        (EventKey (SpecialKey KeyRight) Down _ _) ->
          let p = player glossState
              mz = maze glossState
              nextX = playerX p + cellSize
              nextY = playerY p
          in if isWalkable mz nextX nextY
             then glossState { player = p { playerX = nextX, playerY = nextY, playerAngle = 90 } }
             else glossState

        _ -> glossState

    SolvingEnigma ->
      let enigmaSt = enigmaState glossState
          updatedEnigmaState = enigmaEventHandler event enigmaSt
          newState = initialGlossState (mazeIndex glossState + 1)
      in case event of
           EventKey (SpecialKey KeyEnter) Down _ _ ->
             if selectedOption enigmaSt == correctAnswer enigmaSt
             then newState { gameState = Playing }
             else glossState { gameState = GameOver }
           _ -> glossState { enigmaState = updatedEnigmaState }

    GameOver -> glossState -- Lógica para a tela de Game Over
    Won -> glossState -- Lógica para a tela de vitória

glossTimeHandler :: Float -> GlossState -> GlossState
glossTimeHandler dt glossState =
  let p = player glossState
      m = minotaur glossState
      mazeGrid = maze glossState
      delay = delayTime m
      speed = 1 * cellSize
      hasWon = hasPlayerWon mazeGrid p
      newDelay = delay - dt
      updatedMinotaur = if newDelay <= 0
                        then (moveTowards m p) { delayTime = 1.5 }
                        else m { delayTime = newDelay }
      playerMinotaurCollision = playerX p == minotaurX m && playerY p == minotaurY m
      newState = initialGlossState (mazeIndex glossState + 1)
  in case gameState glossState of
       Playing ->
         if hasWon
         then newState { gameState = SolvingEnigma }
         else if playerMinotaurCollision
                then glossState { gameState = GameOver }
                else glossState { minotaur = updatedMinotaur }
       SolvingEnigma -> glossState
       GameOver -> glossState -- Adicione lógica para o game over
       Won -> glossState -- Adicione lógica para quando o jogador vencer



-- Define o estado inicial do jogo
initialGlossState :: Int -> GlossState
initialGlossState index =
  let maze = mazes !! index
      enigmaIndex = index
      mazeWidth = width maze
      mazeHeight = height maze
      screenWidth = (mazeWidth + 4) * round cellSize
      screenHeight = (mazeHeight + 2) * round cellSize
      mazeOffsetX = calculateMazeOffsetX screenWidth
      mazeOffsetY = calculateMazeOffsetY screenHeight

  in GlossState
      { player = initialPlayerState mazeOffsetX mazeOffsetY cellSize
      , minotaur = initialMinotaurState mazeOffsetX mazeOffsetY cellSize
      , maze = maze
      , time = 0
      , mazeIndex = index
      , gameState = Playing -- Adicione isso para inicializar com o estado de jogo correto
      , enigmaState = initialEnigmaState enigmaIndex -- Adicione isso para inicializar o estado do enigma
      }
