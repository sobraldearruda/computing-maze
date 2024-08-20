module Utils (toContinue, printTextScreen, drawTextScreen, fps, blinkyScatterTarget, pinkyScatterTarget, chaseLength, Characters(..), Boss(..), MazeGame(..), GameStatus(..), Name(..), Direction(..), Mode(..)) where

import Graphics.Gloss.Data.Point

-- Continua o fluxo de execução
toContinue :: IO ()
toContinue = do
    putStrLn "\n### Pressione ENTER para continuar. ###"
    line <- getLine :: IO String
    return()

-- Exibe textos na tela
printTextScreen :: [String] -> IO ()
printTextScreen texts = do
  mapM_ putStrLn texts
  toContinue

-- Concatena listas de strings na tela
drawTextScreen :: [String] -> [String]
drawTextScreen texts = texts

-- | Velocidade do Boss e do MazeGame. Usado para movimento
type Velocity = (Float, Float)

-- | FPS do jogo. O padrão é 60
fps :: Int
fps = 60

-- | Quanto tempo os Boss entrarão em perseguição
chaseLength :: Float
chaseLength = 4.0

-- | Dados que descrevem os diferentes nomes dos personagens do computing-maze
data Name = MazeGame | Pinky | Blinky deriving Eq

-- | Dados que descrevem as direções do MazeGame
data Direction = UP | DOWN | LEFT | RIGHT | STOP deriving (Eq, Show, Ord, Enum, Bounded)

-- | Dados que descrevem vitórias/perdas
data GameStatus = WON | LOST | PLAYING deriving (Eq, Show)

-- | Dados para modos Boss (Dispersão, Perseguir, Com medo)
data Mode = SCATTER | CHASE | FRIGHTENED deriving (Eq, Show)

-- | Canto do Blinky no mapa
blinkyScatterTarget :: Point
blinkyScatterTarget = (8,10)

-- | Canto de Pinky no mapa
pinkyScatterTarget :: Point
pinkyScatterTarget = (-8,10)

-- | Dados que descrevem MazeGame
data Characters = Characters 
    { cName :: Name             -- Nome do personagem dos tipos de nome
    , speed :: Velocity         -- Velocidade constante do personagem
    , location :: Point         -- Localização do personagem em forma de ponto
    } deriving Eq

-- | Dados que descrevem os Boss (Blinky, Pinky)
data Boss = Boss
    { gName :: Name
    , gSpeed :: Velocity
    , gLocation :: Point
    , gTarget :: Point
    , gLastMove :: Direction
    , gDirection :: Direction
    }

-- | Dados que descrevem o estado do jogo
data MazeGame = Game
    { lives :: Int               -- Número de vidas que o MazeGame tem
    , gameStatus :: GameStatus   -- Status do jogo atual (GANHO/PERDA)
    , mazeGame :: Characters       -- Dados do personagem MazeGame
    , pinky :: Boss             -- Dados do personagem Pinky        
    , blinky :: Boss            -- Dados do personagem Blinky 
    , time :: Float              -- Usado para determinar quando liberar os outros 3 caracteres
    , score :: Int               -- Mantém o controle de quantas pelotas foram consumidas!
    , pellets :: [Point]         -- Lista de locais de pellets no labirinto
    , direction :: Direction     -- Direção atual do jogador
    , bufDirection :: Direction  -- Direção de movimento armazenada
    , gMode :: Mode              -- Modo dos Bosses, que pode ser Dispersar, Perseguir, Amedrontado
    , lcrB :: Point              -- Última encruzilhada para Blinky
    , lcrP :: Point              -- Última encruzilhada para Pinky
    , pPellets :: [Point]        -- Lista das 4 localizações de super pastilhas no labirinto
    , fTime :: Float             -- Tempo que os Bosses ficam no modo amedrontado (5 segundos)
    }
