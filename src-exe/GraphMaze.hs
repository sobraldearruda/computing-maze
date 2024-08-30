-- import System.Random
-- import Data.Array
-- import Data.List (delete)

-- type Maze = Array (Int, Int) Int

<<<<<<< HEAD
-- data Direction = North | South | East | West deriving (Enum, Bounded, Show)

-- move :: (Int, Int) -> Direction -> (Int, Int)
-- move (x, y) North = (x, y - 2)
-- move (x, y) South = (x, y + 2)
-- move (x, y) East  = (x + 2, y)
-- move (x, y) West  = (x - 2, y)

-- breakWall :: (Int, Int) -> Direction -> (Int, Int)
-- breakWall (x, y) North = (x, y - 1)
-- breakWall (x, y) South = (x, y + 1)
-- breakWall (x, y) East  = (x + 1, y)
-- breakWall (x, y) West  = (x - 1, y)

-- initMaze :: Int -> Int -> Maze
-- initMaze w h = array ((1, 1), (w, h)) [((x, y), 1) | x <- [1..w], y <- [1..h]]

-- generateMaze :: StdGen -> Maze -> (Int, Int) -> Maze
-- generateMaze gen maze start = go gen maze [start]
--   where
--     go _ maze [] = maze
--     go g m (current:stack) =
--         let directions = shuffle g [minBound .. maxBound]
--             (newGen, _) = split g
--             tryMove m [] _ = go newGen m stack
--             tryMove m (d:ds) (x, y) =
--                 let nextPos = move (x, y) d
--                 in if inBounds nextPos (bounds m) && m ! nextPos == 1
--                    then go newGen (carveMaze m (x, y) nextPos d) (nextPos : (x, y) : stack)
--                    else tryMove m ds (x, y)
--         in tryMove m directions current
=======
-- Define tipos para representar elementos do labirinto
data ElementType = Space | Wall | Marked | Visited | Door
    deriving (Eq) -- permite que seja usado == e != para comparações

-- Define as direções para se mover no labirinto
data Direction = DLeft | DRight | DUp | DDown
    deriving (Enum)

-- Define a representação do labirinto
data MazeData = MazeData {
    width :: Int, -- largura
    height :: Int, -- altura
    gen :: StdGen, -- gerador do labirinto
    maze :: Map (Int, Int) ElementType -- armazena o labirinto, associa coordenadas a um tipo
}

-- Permite que os valores sejam convertidos em strings para exibição
instance Show ElementType where
    show Space = "  " -- dois espaços em branco
    show Wall = "[]" -- dois colchetes
    show Marked = "++" -- dois símbolos de adição
    show Visited = show Space -- dois espaços em branco
    show Door = show Space -- dois espaços em branco

-- Permite que os valores sejam convertidos em strings para exibição
instance Show MazeData where
    show = printMazeMatrix -- converte o labirinto para uma representação textual
>>>>>>> 2c9c7ebc9556245627269272c0d952e2e17ae3d5

-- carveMaze :: Maze -> (Int, Int) -> (Int, Int) -> Direction -> Maze
-- carveMaze maze pos nextPos dir =
--     let breakPos = breakWall pos dir
--     in maze // [(pos, 0), (breakPos, 0), (nextPos, 0)]

-- shuffle :: RandomGen g => g -> [a] -> [a]
-- shuffle _ [] = []
-- shuffle gen xs =
--     let (n, newGen) = randomR (0, length xs - 1) gen
--         (first, x:rest) = splitAt n xs
--     in x : shuffle newGen (first ++ rest)

-- inBounds :: (Int, Int) -> ((Int, Int), (Int, Int)) -> Bool
-- inBounds (x, y) ((xmin, ymin), (xmax, ymax)) = x >= xmin && x <= xmax && y >= ymin && y >= xmin && y <= ymax

-- printMaze :: Maze -> IO ()
-- printMaze maze = do
--     let ((xmin, ymin), (xmax, ymax)) = bounds maze
--     mapM_ (\y -> do
--               mapM_ (\x -> putStr (if maze ! (x, y) == 1 then "[]" else "  ")) [xmin..xmax]
--               putStrLn ""
--           ) [ymin..ymax]

-- main :: IO ()
-- main = do
--     let width = 40
--     let height = 50
--     let maze = initMaze width height
--     gen <- getStdGen
--     let generatedMaze = generateMaze gen maze (2, 2)
--     let mazeWithEntryExit = generatedMaze // [((1, 2), 0), ((width - 2, height - 3), 0)]
--     printMaze mazeWithEntryExit

