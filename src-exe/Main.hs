-- module Main where

-- import Graphics.Gloss
-- import Graphics.Gloss.Interface.Pure.Game

-- type State = (Float, Float, Float) -- x, y, angle
-- type GlossState = (State, Picture)
-- type Maze = [[Int]]

-- initialState :: State
-- initialState = (0, 0, 90)

-- initialGlossState :: Picture -> GlossState
-- initialGlossState z = (initialState, z)

-- glossEventHandler :: Event -> GlossState -> GlossState
-- glossEventHandler (EventKey (SpecialKey KeyUp) Down _ _)    ((x, y, _), z) = ((x, y+20, 0), z)
-- glossEventHandler (EventKey (SpecialKey KeyDown) Down _ _)  ((x, y, _), z) = ((x, y-20, -180), z)
-- glossEventHandler (EventKey (SpecialKey KeyLeft) Down _ _)  ((x, y, _), z) = ((x-20, y, -90), z)
-- glossEventHandler (EventKey (SpecialKey KeyRight) Down _ _) ((x, y, _), z) = ((x+20, y, 90), z)  
-- glossEventHandler _ s = s

-- glossTimeHandler :: Float -> GlossState -> GlossState
-- glossTimeHandler n ((x, y, a), z) = ((x, y, a), z)

-- fr :: Int 
-- fr = 50

-- dm :: Display
-- dm = InWindow "Computing Maze" (800,600) (300,50)

-- drawGlossState :: GlossState -> Picture
-- drawGlossState ((x, y, angle), z) = Translate x y (Rotate angle z)

-- triangulo :: Path
-- triangulo = [(0, 100), (-87, -50), (87, -50)]

-- trianguloIsosceles :: Path
-- trianguloIsosceles = [(0, 100), (-50, -50), (50, -50)]



-- main :: IO()
-- main = do
--   let player = scale 0.5 0.5 $ color red (polygon trianguloIsosceles)
--   play dm
--     black
--     fr
--     (initialGlossState player)
--     drawGlossState
--     glossEventHandler
--     glossTimeHandler

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

type State = (Float, Float, Float) -- x, y, angle
type Maze = [[Int]]
type GlossState = (State, Picture, Maze)

-- Exemplo de um labirinto: 1 representa uma parede e 0 espaço livre.
exampleMaze :: Maze
exampleMaze = [ [1, 1, 1, 1, 1]
              , [1, 0, 0, 0, 1]
              , [1, 0, 1, 0, 1]
              , [1, 0, 0, 0, 1]
              , [1, 1, 1, 1, 1]
              ]

initialState :: State
initialState = (0, 0, 90)

initialGlossState :: Picture -> Maze -> GlossState
initialGlossState player maze = (initialState, player, maze)

glossEventHandler :: Event -> GlossState -> GlossState
glossEventHandler (EventKey (SpecialKey KeyUp) Down _ _)    ((x, y, _), player, maze) = ((x, y+20, 0), player, maze)
glossEventHandler (EventKey (SpecialKey KeyDown) Down _ _)  ((x, y, _), player, maze) = ((x, y-20, -180), player, maze)
glossEventHandler (EventKey (SpecialKey KeyLeft) Down _ _)  ((x, y, _), player, maze) = ((x-20, y, -90), player, maze)
glossEventHandler (EventKey (SpecialKey KeyRight) Down _ _) ((x, y, _), player, maze) = ((x+20, y, 90), player, maze)
glossEventHandler _ s = s

glossTimeHandler :: Float -> GlossState -> GlossState
glossTimeHandler _ s = s

fr :: Int 
fr = 50

dm :: Display
dm = InWindow "Computing Maze" (800, 600) (300, 50)

drawMaze :: Maze -> Picture
drawMaze maze = Pictures [translateToCell x y | (y, row) <- zip [0..] maze, (x, cell) <- zip [0..] row, cell == 1]
  where
    translateToCell x y = Translate (fromIntegral x * 40) (fromIntegral (-y) * 40) (color blue (rectangleSolid 40 40))

drawGlossState :: GlossState -> Picture
drawGlossState ((x, y, angle), player, maze) =
  Pictures [drawMaze maze, Translate x y (Rotate angle player)]

triangulo :: Path
triangulo = [(0, 100), (-87, -50), (87, -50)]

trianguloIsosceles :: Path
trianguloIsosceles = [(0, 100), (-50, -50), (50, -50)]

main :: IO()
main = do
  let player = scale 0.5 0.5 $ color red (polygon trianguloIsosceles)
  play dm
    black
    fr
    (initialGlossState player exampleMaze)
    drawGlossState
    glossEventHandler
    glossTimeHandler

    