module Player (Player (..), initialPlayerState) where

data Player = Player
    { playerX     :: Float
    , playerY     :: Float
    , playerAngle :: Float
    } deriving (Show, Eq)

initialPlayerState :: Float -> Float-> Float -> Player
initialPlayerState mazeOffsetX mazeOffsetY cellSize = Player 
    {
        playerX = mazeOffsetX + cellSize
    ,   playerY = mazeOffsetX - cellSize
    ,   playerAngle = 90
    }