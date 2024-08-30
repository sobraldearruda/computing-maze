module Minotaur (Minotaur (..), initialMinotaurState) where

data Minotaur = Minotaur
    { minotaurX         :: Float
    , minotaurY         :: Float
    , minotaurAngle     :: Float
    , delayTime :: Float
    } deriving (Show, Eq)

initialMinotaurState :: Float -> Float-> Float -> Minotaur
initialMinotaurState mazeOffsetX mazeOffsetY cellSize = Minotaur 
    {
        minotaurX = mazeOffsetX - cellSize
    ,   minotaurY = mazeOffsetX - cellSize
    ,   minotaurAngle = 90
    ,   delayTime = 1.5
    }