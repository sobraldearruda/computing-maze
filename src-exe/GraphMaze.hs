module Main where

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Data.Map
import System.Random

data ElementType = Space | Wall | Marked | Visited | Door
    deriving (Eq)

data Direction = DLeft | DRight | DUp | DDown
    deriving (Enum)

data MazeData = MazeData {
    width :: Int,
    height :: Int,
    gen :: StdGen,
    maze :: Map (Int, Int) ElementType
}

instance Show ElementType where
    show Space = "  "
    show Wall = "[]"
    show Marked = "++"
    show Visited = show Space
    show Door = show Space

instance Show MazeData where
    show = printMazeMatrix

-- Função para imprimir o labirinto como uma matriz
printMazeMatrix :: MazeData -> String
printMazeMatrix md =
    unlines [concat [showElement (x, y) | x <- [0 .. width md - 1]] | y <- [0 .. height md - 1]]
    where
        showElement pos = findWithDefault Wall pos (maze md) |> show

type MazeState a = ReaderT (Int, Int) (State MazeData) a

-- Inicializa um labirinto não esculpido.
initMaze :: Int -> Int -> Int -> MazeData
initMaze w h s =
    let xs = [0 .. w - 1] in
    let ys = [0 .. h - 1] in
    let top = [(x, 0) | x <- xs] in
    let bottom = [(x, h - 1) | x <- xs] in
    let left = [(0, y) | y <- ys] in
    let right = [(w - 1, y) | y <- ys] in
    let start = [(2, 2)] in
    let spaces = Prelude.foldl (++) [] [top, bottom, left, right, start] in
    let m1 = fromList [(p, Space) | p <- spaces] in
    let doors = [(2, 2), (2, 1), (w - 3, h - 2)] in
    let m2 = fromList [(p, Door) | p <- doors] in
    MazeData {
        width = w, height = h, gen = mkStdGen s,
        maze = union m1 m2
    }

-- Obtém a próxima posição na direção especificada.
getPosition :: (Int, Int) -> Direction -> (Int, Int)
getPosition (x, y) DLeft = (x - 1, y)
getPosition (x, y) DRight = (x + 1, y)
getPosition (x, y) DUp = (x, y - 1)
getPosition (x, y) DDown = (x, y + 1)

-- Move na direção especificada.
move :: ElementType -> Direction -> MazeState (Int, Int)
move fill d = do
    pos <- ask; st <- get
    let mid = getPosition pos d
    let m = insert mid fill $ maze st
    let next = getPosition mid d
    put $ st { maze = insert next fill m }
    return next

-- Determina se podemos esculpir na direção especificada.
canMove :: ElementType -> Direction -> MazeState Bool
canMove match d = do
    pos <- ask; st <- get
    let mid = getPosition pos d
    let e1 = findWithDefault Wall mid $ maze st
    let next = getPosition mid d
    let e2 = findWithDefault Wall next $ maze st
    return $ e1 == match && e2 == match

-- Esculpe o labirinto a partir da posição inicial.
carve :: MazeState ()
carve = do
    st <- get
    let (start, g') = randomR (0, 3) $ gen st
    put $ st { gen = g' }
    mapM_ tryCarve [toEnum ((start + i) `mod` 4) | i <- [0 .. 3]]
    where
        tryCarve d = do
            cm <- canMove Wall d
            when cm (move Space d >>= (\n -> local (const n) carve))

-- Encontra um caminho do ponto inicial até o ponto final.
solveTo :: (Int, Int) -> MazeState Bool
solveTo stop = do
    pos <- ask
    if pos == stop then
        return True
    else do
        st <- get; pos <- ask
        put $ st { maze = insert pos Marked (maze st) }
        foldM trySolve False [toEnum i | i <- [0 .. 3]]
    where
        trySolve result d
            | result = return True
            | otherwise = do
                cm <- canMove Space d
                if cm then do
                    next <- move Marked d
                    found <- local (const next) $ solveTo stop
                    if found then return True
                    else (move Visited d >> return False)
                else return False

-- Gera um labirinto aleatório.
generate :: Int -> Int -> Int -> MazeData
generate w h s = execState (runReaderT carve (2, 2)) (initMaze w h s)

-- Resolve um labirinto.
solve :: MazeData -> MazeData
solve md =
    let start = (2, 2) in
    let stop = ((width md) - 3, (height md - 3)) in
    execState (runReaderT (solveTo stop) start) md

-- Gera e exibe um labirinto resolvido na forma de matriz.
main :: IO ()
main = do
    let maze = generate 39 23 5
    let solved = solve maze
    putStrLn $ printMazeMatrix maze
    putStrLn $ printMazeMatrix solved
