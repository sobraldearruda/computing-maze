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

-- Tipo de dado 'GameState', que define os possíveis estados do jogo:
-- 'Playing': Jogador está jogando no labirinto.
-- 'SolvingEnigma': Jogador está resolvendo um enigma.
-- 'GameOver': O jogo terminou em derrota.
-- 'Won': O jogo terminou em vitória.
data GameState = Playing | SolvingEnigma | GameOver | Won deriving (Eq, Show)

-- Tipo de dado 'GlossState', que representa o estado geral do jogo, incluindo:
-- 'player': Estado do jogador.
-- 'minotaur': Estado do minotauro.
-- 'maze': O labirinto atual.
-- 'mazeIndex': Índice do labirinto atual.
-- 'time': Tempo decorrido no jogo.
-- 'gameState': Estado atual do jogo (jogando, resolvendo enigma, game over ou vitória).
-- 'enigmaState': Estado atual do enigma que está sendo resolvido.
data GlossState = GlossState
  { player :: Player
  , minotaur :: Minotaur
  , maze :: Maze
  , mazeIndex :: Int
  , time :: Int
  , gameState :: GameState
  , enigmaState :: EnigmaState
  } deriving (Eq, Show)

-- Define a taxa de quadros por segundo do jogo.
fr :: Int
fr = 50

-- Recebe um 'GlossState' e desenha o estado atual do jogo na tela.
-- Dependendo do estado do jogo, ele desenha o labirinto com o jogador e o minotauro,
-- o enigma que está sendo resolvido, ou mensagens de 'Game Over' ou 'You Won'.
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

-- Lida com os eventos do jogo, como movimentos do jogador ou interações com enigmas,
-- e transforma o estado atual em um novo 'GlossState'. Dependendo do estado do jogo, ele pode mover o jogador,
-- resolver enigmas ou determinar se o jogador ganhou ou perdeu.
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
          maxIndex = 8  -- Supondo que tem-se 9 enigmas, índices de 0 a 8.
          newState = initialGlossState (mazeIndex glossState + 1)
      in case event of
           EventKey (SpecialKey KeyEnter) Down _ _ ->
             if selectedOption enigmaSt == correctAnswer enigmaSt
             then if mazeIndex glossState == maxIndex
                    then glossState { gameState = Won }
                    else newState { gameState = Playing }
             else glossState { gameState = GameOver }
           _ -> glossState { enigmaState = updatedEnigmaState }

    GameOver -> glossState -- Lógica para a tela de game over.
    Won -> glossState -- Lógica para a tela de vitória.

-- Lida com o tempo decorrido no jogo. Ele é responsável por
-- mover o minotauro em direção ao jogador a cada intervalo de tempo, verificar se o jogador ganhou
-- ou perdeu, e atualizar o estado do jogo conforme necessário.
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
      newState = initialGlossState (mazeIndex glossState)
  in case gameState glossState of
       Playing ->
         if hasWon
         then newState { gameState = SolvingEnigma }
         else if playerMinotaurCollision
                then glossState { gameState = GameOver }
                else glossState { minotaur = updatedMinotaur }
       SolvingEnigma -> glossState
       GameOver -> glossState -- Lógica para o game over.
       Won -> glossState -- Lógica para a vitória.

-- Define o estado inicial do jogo. Ele inicializa o labirinto, 
-- o jogador, o minotauro, e o estado do enigma com base no índice do labirinto fornecido.
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
      , gameState = Playing -- Lógica para inicializar com o estado de jogo correto.
      , enigmaState = initialEnigmaState enigmaIndex -- Lógica para inicializar o estado do enigma.
      }
