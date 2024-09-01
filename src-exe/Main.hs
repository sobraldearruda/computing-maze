-- module Main where
-- import Graphics.Gloss
-- import Graphics.Gloss.Interface.Pure.Game
-- import PrefabMazes (maze21x21, exampleMaze3, exampleMaze)

-- type State = (Float, Float, Float) -- x, y, angle
-- type Maze = [[Int]]
-- type GlossState = (State, Picture, Maze, Int, Int) -- Adicione as dimensões do labirinto

-- scaleFactor :: Float
-- scaleFactor = 0.7

-- -- Defina o tamanho da célula do labirinto
-- cellSize :: Float
-- cellSize = 40 * scaleFactor

-- -- Calcule o deslocamento para centralizar o labirinto na tela
-- calculateMazeOffsetX, calculateMazeOffsetY :: Int -> Int -> Float
-- calculateMazeOffsetX mazeWidth screenWidth = -fromIntegral screenWidth / 2 + 2.5 * cellSize
-- calculateMazeOffsetY mazeHeight screenHeight = fromIntegral screenHeight / 2 - (1.5 * cellSize)

-- -- Calcule a posição inicial do jogador para a célula (1,1)
-- initialPlayerX, initialPlayerY :: Float -> Float
-- initialPlayerX mazeOffsetX = mazeOffsetX + cellSize
-- initialPlayerY mazeOffsetY = mazeOffsetY - cellSize

-- -- Defina a posição inicial do jogador
-- initialState :: Float -> Float -> State
-- initialState playerX playerY = (playerX, playerY, 90)


-- -- Inicializa o estado do jogo
-- -- Recebe uma imagem do jogador e do labirinto
-- -- Retorna uma tupla com o estado inicial, o jogador e o labirinto
-- initialGlossState :: Picture -> Maze -> GlossState
-- initialGlossState player maze =
--   let mazeWidth = length (head maze)
--       mazeHeight = length maze
--       screenWidth = (mazeWidth + 4) * round cellSize
--       screenHeight = (mazeHeight + 2) * round cellSize
--       mazeOffsetX = calculateMazeOffsetX mazeWidth screenWidth
--       mazeOffsetY = calculateMazeOffsetY mazeHeight screenHeight
--       playerX = initialPlayerX mazeOffsetX
--       playerY = initialPlayerY mazeOffsetY
--   in (initialState playerX playerY, player, maze, mazeWidth, mazeHeight)

-- -- Recebe um evento e um estado do jogo
-- -- Retorna um novo estado do jogo
-- glossEventHandler :: Event -> GlossState -> GlossState
-- glossEventHandler (EventKey (SpecialKey KeyUp) Down _ _)    ((x, y, angle), player, maze, mw, mh) = ((x, y + cellSize, 0), player, maze, mw, mh)
-- glossEventHandler (EventKey (SpecialKey KeyDown) Down _ _)  ((x, y, angle), player, maze, mw, mh) = ((x, y - cellSize, -180), player, maze, mw, mh)
-- glossEventHandler (EventKey (SpecialKey KeyLeft) Down _ _)  ((x, y, angle), player, maze, mw, mh) = ((x - cellSize, y, -90), player, maze, mw, mh)
-- glossEventHandler (EventKey (SpecialKey KeyRight) Down _ _) ((x, y, angle), player, maze, mw, mh) = ((x + cellSize, y, 90), player, maze, mw, mh)
-- glossEventHandler _ s = s

-- glossTimeHandler :: Float -> GlossState -> GlossState
-- glossTimeHandler _ s = s -- no momento, esta função não faz nada, está retornando o mesmo estado

-- -- Representa a taxa de quadros (frames) por segundo
-- fr :: Int 
-- fr = 50

-- -- Ajuste a função drawMaze para pintar as paredes de branco e o caminho de preto
-- drawMaze :: Maze -> Float -> Float -> Picture
-- drawMaze maze mazeOffsetX mazeOffsetY =
--   Translate mazeOffsetX mazeOffsetY (Pictures [drawCell x y cell | (y, row) <- zip [0..] maze, (x, cell) <- zip [0..] row])
--   where
--     drawCell x y cell
--       | cell == 1 = Translate (fromIntegral x * cellSize) (fromIntegral (-y) * cellSize) (color black (rectangleSolid cellSize cellSize))
--       | otherwise = Translate (fromIntegral x * cellSize) (fromIntegral (-y) * cellSize) (color white (rectangleSolid cellSize cellSize))

-- -- Recebe um estado do jogo (tupla com o estado do jogador, o jogador e o labirinto)
-- -- Retorna uma imagem combinada
-- drawGlossState :: GlossState -> Picture
-- drawGlossState ((x, y, angle), player, maze, mazeWidth, mazeHeight) =
--   let screenWidth = (mazeWidth + 4) * round cellSize
--       screenHeight = (mazeHeight + 2) * round cellSize
--       mazeOffsetX = calculateMazeOffsetX mazeWidth screenWidth
--       mazeOffsetY = calculateMazeOffsetY mazeHeight screenHeight
--   in Pictures [drawMaze maze mazeOffsetX mazeOffsetY, Translate x y (Rotate angle player)]

-- -- Defina o sprite do jogador para ter o mesmo tamanho que uma célula do labirinto
-- triangulo :: Path
-- triangulo = [(0, cellSize / 2), (-cellSize / 2, -cellSize / 2), (cellSize / 2, -cellSize / 2)]

-- -- Um triângulo isósceles com coordenadas
-- -- Primeira tupla: topo do triângulo
-- -- Segunda e terceira tuplas: bases do triângulo
-- trianguloIsosceles :: Path
-- trianguloIsosceles = [(0, cellSize / 2), (-cellSize / 2, -cellSize / 2), (cellSize / 2, -cellSize / 2)]

-- main :: IO()
-- main = do
--   let player = scale (cellSize / cellSize) (cellSize / cellSize) $ color red (polygon trianguloIsosceles)
--       exampleMaze = maze21x21  -- Use o labirinto que desejar
--       mazeWidth = length (head exampleMaze)
--       mazeHeight = length exampleMaze
--       screenWidth = (mazeWidth + 4) * round cellSize
--       screenHeight = (mazeHeight + 2) * round cellSize
--       dm = InWindow "Computing Maze" (screenWidth, screenHeight) (300, 50)
--   play dm
--     black -- Fundo da tela preto
--     fr
--     (initialGlossState player exampleMaze3)
--     drawGlossState
--     glossEventHandler
--     glossTimeHandler

-- =================================================================================================

-- module Main where
-- import Graphics.Gloss
-- import Graphics.Gloss.Interface.Pure.Game
-- import Minotaur (Minotaur (..), initialMinotaurState)
-- import Maze (Maze (..), maze21x21, exampleMaze3)
-- import Player (Player (..), initialPlayerState)

-- data GlossState = GlossState
--     { player    :: Player    -- Estado do jogador
--     , minotaur  :: Minotaur  -- Estado do minotauro
--     , maze      :: Maze
--     , time      :: Float  -- Matriz do labirinto
--     } deriving (Show, Eq)

-- scaleFactor :: Float
-- scaleFactor = 0.7

-- -- Defina o tamanho da célula do labirinto
-- cellSize :: Float
-- cellSize = 40 * scaleFactor

-- -- Calcule o deslocamento para centralizar o labirinto na tela
-- calculateMazeOffsetX, calculateMazeOffsetY :: Int -> Float
-- calculateMazeOffsetX screenWidth = -fromIntegral screenWidth / 2 + 2.5 * cellSize
-- calculateMazeOffsetY screenHeight = fromIntegral screenHeight / 2 - (1.5 * cellSize)

-- -- Defina a função para criar o estado inicial
-- initialGlossState :: Maze -> GlossState
-- initialGlossState maze =
--   let mazeWidth = width maze
--       mazeHeight = height maze
--       screenWidth = (mazeWidth + 4) * round cellSize
--       screenHeight = (mazeHeight + 2) * round cellSize
--       mazeOffsetX = calculateMazeOffsetX screenWidth
--       mazeOffsetY = calculateMazeOffsetY screenHeight

--   in GlossState
--       { player = initialPlayerState mazeOffsetX mazeOffsetY cellSize
--       , minotaur = initialMinotaurState mazeOffsetX mazeOffsetY cellSize
--       , maze = maze
--       , time = 0  -- Inicialize o tempo acumulado
--       }

-- -- Atualiza o estado do Gloss com base no evento
-- glossEventHandler :: Event -> GlossState -> GlossState
-- glossEventHandler (EventKey (SpecialKey KeyUp) Down _ _) glossState =
--     let p = player glossState
--         updatedPlayer = p { playerX = playerX p, playerY = playerY p + cellSize, playerAngle = 0 }
--     in glossState { player = updatedPlayer }

-- glossEventHandler (EventKey (SpecialKey KeyDown) Down _ _) glossState =
--     let p = player glossState
--         updatedPlayer = p { playerX = playerX p, playerY = playerY p - cellSize, playerAngle = -180 }
--     in glossState { player = updatedPlayer }

-- glossEventHandler (EventKey (SpecialKey KeyLeft) Down _ _) glossState =
--     let p = player glossState
--         updatedPlayer = p { playerX = playerX p - cellSize, playerY = playerY p, playerAngle = -90 }
--     in glossState { player = updatedPlayer }

-- glossEventHandler (EventKey (SpecialKey KeyRight) Down _ _) glossState =
--     let p = player glossState
--         updatedPlayer = p { playerX = playerX p + cellSize, playerY = playerY p, playerAngle = 90 }
--     in glossState { player = updatedPlayer }

-- glossEventHandler _ glossState = glossState

-- moveTowards :: Minotaur -> Player -> Minotaur
-- moveTowards minotaur player 
--   | dx > dy   = minotaur {minotaurX = x1 + signum (x2 - x1) * cellSize, minotaurY = y1}
--   | otherwise = minotaur {minotaurX = x1, minotaurY = y1 + signum (y2 - y1) * cellSize}
--   where
--     x1 = minotaurX minotaur 
--     y1 = minotaurY minotaur
--     x2 = playerX player 
--     y2 = playerY player
--     dx = abs (x2 - x1)
--     dy = abs (y2 - y1)

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

-- fr :: Int
-- fr = 50

-- -- Ajuste a função drawMaze para pintar as paredes de branco e o caminho de preto
-- drawMaze :: Maze -> Float -> Float -> Picture
-- drawMaze maze mazeOffsetX mazeOffsetY =
--   Translate mazeOffsetX mazeOffsetY (Pictures [drawCell x y cell | (y, row) <- zip [0..] (grid maze), (x, cell) <- zip [0..] row])
--   where
--     drawCell x y cell
--       | cell == 1 = Translate (fromIntegral x * cellSize) (fromIntegral (-y) * cellSize) (color black (rectangleSolid cellSize cellSize))
--       | otherwise = Translate (fromIntegral x * cellSize) (fromIntegral (-y) * cellSize) (color white (rectangleSolid cellSize cellSize))

-- drawGlossState :: GlossState -> Picture
-- drawGlossState glossState =
--   let mazeState = maze glossState
--       minotaurState = minotaur glossState
--       playerState = player glossState
--       screenWidth = (width mazeState + 4) * round cellSize
--       screenHeight = (height mazeState + 2) * round cellSize
--       mazeOffsetX = calculateMazeOffsetX screenWidth
--       mazeOffsetY = calculateMazeOffsetY screenHeight
--       minotaur' = Translate (minotaurX minotaurState) (minotaurY minotaurState) (Rotate (minotaurAngle minotaurState) (color blue (rectangleSolid cellSize cellSize)))
--       player' = Translate (playerX playerState) (playerY playerState) (Rotate (playerAngle playerState) (color red (rectangleSolid cellSize cellSize))) -- Cor da peça do jogador ajustada para vermelho, por exemplo
--   in Pictures [drawMaze mazeState mazeOffsetX mazeOffsetY, player', minotaur']


-- -- Defina o sprite do jogador para ter o mesmo tamanho que uma célula do labirinto
-- triangulo :: Path
-- triangulo = [(0, cellSize / 2), (-cellSize / 2,-cellSize / 2), (cellSize / 2,-cellSize / 2)]

-- main :: IO()
-- main = do
--   let player = scale (cellSize / cellSize) (cellSize / cellSize) $ color red (polygon triangulo)
--       exampleMaze = maze21x21  -- Use o labirinto que desejar
--       mazeWidth = width exampleMaze
--       mazeHeight = height exampleMaze
--       screenWidth = (mazeWidth + 4) * round cellSize
--       screenHeight = (mazeHeight + 2) * round cellSize
--       dm = InWindow "Computing Maze" (screenWidth, screenHeight) (300, 50)
--       initialState = initialGlossState exampleMaze3
--   play dm
--     black -- Fundo da tela preto
--     fr
--     initialGlossState
--     drawGlossState
--     glossEventHandler
--     glossTimeHandler
-- =============================================================================================

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Menu
import Player (Player(..), initialPlayerState, triangulo)
import Minotaur (Minotaur(..), initialMinotaurState)
import Maze (Maze(..), cellSize, maze9)
import Rendering (GlossState(..), drawGlossState, glossEventHandler, glossTimeHandler, initialGlossState, fr)

-- Estado do jogo
data AppState = AppState
  { menuState :: MenuState
  , gameState :: Maybe GlossState
  } deriving (Eq, Show)

-- Função que desenha o estado atual do aplicativo
drawAppState :: AppState -> Picture
drawAppState (AppState MainMenu _) = renderMenu MainMenu
drawAppState (AppState PlayGame (Just gs)) = drawGlossState gs
<<<<<<< Updated upstream
drawAppState _ = Blank
=======
drawAppState (AppState Tutorial _) = renderMenu Tutorial
drawAppState (AppState ExitGame _) = Blank -- Placeholder para sair do jogo
>>>>>>> Stashed changes

-- Função que lida com os eventos do aplicativo
appEventHandler :: Event -> AppState -> AppState
appEventHandler event (AppState MainMenu Nothing) =
  let newMenuState = menuEventHandler(event) MainMenu
  in case newMenuState of
       PlayGame -> AppState PlayGame (Just (initialGlossState maze9))
<<<<<<< Updated upstream
       ExitGame -> AppState ExitGame Nothing
       _        -> AppState newMenuState Nothing
=======
       Tutorial -> AppState Tutorial Nothing
       ExitGame -> AppState ExitGame Nothing
       MainMenu -> AppState MainMenu Nothing
       _        -> AppState newMenuState Nothing -- Manter o menu principal
>>>>>>> Stashed changes

appEventHandler event (AppState PlayGame (Just gs)) =
  AppState PlayGame (Just (glossEventHandler event gs))

<<<<<<< Updated upstream
=======
appEventHandler event (AppState Tutorial _) =
  let newMenuState = menuEventHandler event Tutorial
  in case newMenuState of
       MainMenu -> AppState MainMenu Nothing
       _        -> AppState Tutorial Nothing

>>>>>>> Stashed changes
appEventHandler _ s = s

-- Função que lida com o tempo do aplicativo
appTimeHandler :: Float -> AppState -> AppState
appTimeHandler dt (AppState PlayGame (Just gs)) =
  AppState PlayGame (Just (glossTimeHandler dt gs))

appTimeHandler _ s = s

main :: IO()
main = do
  let screenWidth = 800
      screenHeight = 600
      dm = InWindow "Computing Maze" (screenWidth, screenHeight) (300, 50)
      initialState = AppState MainMenu Nothing
  play dm
    black -- Fundo da tela preto
    fr
    initialState
    drawAppState
    appEventHandler
    appTimeHandler