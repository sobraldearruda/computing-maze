module BossAI (releasePinky, bossModeSwitch, bossCollisionSwitch, moveToTargetB, moveToTargetP, listLength) where

import Utils (fps, pinkyScatterTarget, blinkyScatterTarget, chaseLength, Characters(..), Boss(..), MazeGame(..), Direction(..), Name(..), Mode(..), GameStatus(..))
import Maze
import Graphics.Gloss


-- | Length of a list. Used for the score
listLength :: [a] -> Int
listLength [] = 0
listLength (x:xs) = 1 + listLength xs

-- | Reverse the True and False statements from the standard `elem` function
checkInList :: Eq a => a -> [a] -> Bool
checkInList x l
    | x `elem` l = False
    | otherwise  = True


-- Scatter for 7 seconds, then Chase for 20 seconds.
-- Scatter for 7 seconds, then Chase for 20 seconds.
-- Scatter for 5 seconds, then Chase for 20 seconds.
-- Scatter for 5 seconds, then switch to Chase mode permanently.
-- add 3 seconds to everything because I stop the first 3 seconds of the game for the start screen
-- | Changing from scatter to chase follow MazeGame rules.   
 -- | Changing from scatter to chase follow Pac-Mans rules.   
bossModeTimer :: MazeGame -> MazeGame
bossModeTimer game
                | curTime < 10 = game { gMode = SCATTER
                                                        , blinky = Boss {gName = Blinky, gSpeed = curSpeedB, gLocation = curLocationB, gTarget = blinkyScatterTarget, gLastMove = lmB, gDirection = dB}
                                                        , pinky = Boss {gName = Pinky, gSpeed = curSpeedP, gLocation = curLocationP, gTarget = pinkyScatterTarget, gLastMove = lmP, gDirection = dP}}
                | curTime >= 10 && curTime < 30 = game {gMode = CHASE
                                                        , blinky = Boss {gName = Blinky, gSpeed = curSpeedB, gLocation = curLocationB, gTarget = (x',y'), gLastMove = lmB, gDirection = dB}
                                                        , pinky = Boss {gName = Pinky, gSpeed = curSpeedP, gLocation = curLocationP, gTarget = (x'+3,y'+3), gLastMove = lmP, gDirection = dP}}
                | curTime >= 30 && curTime < 37 = game {gMode = SCATTER
                                                        , blinky = Boss {gName = Blinky, gSpeed = curSpeedB, gLocation = curLocationB, gTarget = blinkyScatterTarget, gLastMove = lmB, gDirection = dB}
                                                        , pinky = Boss {gName = Pinky, gSpeed = curSpeedP, gLocation = curLocationP, gTarget = pinkyScatterTarget, gLastMove = lmP, gDirection = dP}}
                | curTime >= 37 && curTime < 57 = game {gMode = CHASE   
                                                        , blinky = Boss {gName = Blinky, gSpeed = curSpeedB, gLocation = curLocationB, gTarget = (x',y'), gLastMove = lmB, gDirection = dB}
                                                        , pinky = Boss {gName = Pinky, gSpeed = curSpeedP, gLocation = curLocationP, gTarget = (x'+3,y'+3), gLastMove = lmP, gDirection = dP}}
                | curTime >= 57 && curTime < 62 = game {gMode = SCATTER
                                                        , blinky = Boss {gName = Blinky, gSpeed = curSpeedB, gLocation = curLocationB, gTarget = blinkyScatterTarget, gLastMove = lmB, gDirection = dB}
                                                        , pinky = Boss {gName = Pinky, gSpeed = curSpeedP, gLocation = curLocationP, gTarget = pinkyScatterTarget, gLastMove = lmP, gDirection = dP}}
                | curTime >= 62 && curTime < 82 = game {gMode = CHASE
                                                        , blinky = Boss {gName = Blinky, gSpeed = curSpeedB, gLocation = curLocationB, gTarget = (x',y'), gLastMove = lmB, gDirection = dB}
                                                        , pinky = Boss {gName = Pinky, gSpeed = curSpeedP, gLocation = curLocationP, gTarget = (x'+3,y'+3), gLastMove = lmP, gDirection = dP}}
                | curTime >= 82 && curTime < 87 = game {gMode = SCATTER
                                                        , blinky = Boss {gName = Blinky, gSpeed = curSpeedB, gLocation = curLocationB, gTarget = blinkyScatterTarget, gLastMove = lmB, gDirection = dB}
                                                        , pinky = Boss {gName = Pinky, gSpeed = curSpeedP, gLocation = curLocationP, gTarget = pinkyScatterTarget, gLastMove = lmP, gDirection = dP}}
                | curTime >= 87 && curTime < 107 = game {gMode = CHASE
                                                        , blinky = Boss {gName = Blinky, gSpeed = curSpeedB, gLocation = curLocationB, gTarget = (x',y'), gLastMove = lmB, gDirection = dB}
                                                        , pinky = Boss {gName = Pinky, gSpeed = curSpeedP, gLocation = curLocationP, gTarget = (x'+3,y'+3), gLastMove = lmP, gDirection = dP}}
                | curTime >= 107 && curTime < 112 = game {gMode = SCATTER
                                                        , blinky = Boss {gName = Blinky, gSpeed = curSpeedB, gLocation = curLocationB, gTarget = blinkyScatterTarget, gLastMove = lmB, gDirection = dB}
                                                        , pinky = Boss {gName = Pinky, gSpeed = curSpeedP, gLocation = curLocationP, gTarget = pinkyScatterTarget, gLastMove = lmP, gDirection = dP}}
                | otherwise = game {gMode = CHASE
                                                        , blinky = Boss {gName = Blinky, gSpeed = curSpeedB, gLocation = curLocationB, gTarget = (x',y'), gLastMove = lmB, gDirection = dB}
                                                        , pinky = Boss {gName = Pinky, gSpeed = curSpeedP, gLocation = curLocationP, gTarget = (x'+3,y'+3), gLastMove = lmP, gDirection = dP}}
                where
                    curTime = time game
                    (x,y) = (location (mazeGame game))
                    x' = fromIntegral (round x)
                    y' = fromIntegral (round y)

                    --- Blinky ---
                    curLocationB = (gLocation (blinky game))
                    curSpeedB = (gSpeed (blinky game))
                    lmB = (gLastMove (blinky game))
                    dB = (gDirection (blinky game))

                     --- Pinky ---
                    curLocationP = (gLocation (pinky game))
                    curSpeedP = (gSpeed (pinky game))
                    lmP = (gLastMove (pinky game))
                    dP = (gDirection (pinky game))


-- | Determina se o modo dos Bosses deve ser alterado
bossModeSwitch :: MazeGame -> MazeGame
bossModeSwitch game
    | curMode == SCATTER || curMode == CHASE = bossModeTimer game
    | frTime >= chaseLength = bossModeTimer game
    | frTime < chaseLength = game { fTime = frTime + 1 / fromIntegral fps }
    | otherwise = game
  where
    curMode = gMode game
    frTime = fTime game

-- | Função para liberar Pinky no momento apropriado
releasePinky :: MazeGame -> MazeGame
releasePinky game = if curTime > 4 && posPinky == (0, 0)
                    then game { pinky = Boss { gName = Pinky
                                              , gSpeed = (0, 0)
                                              , gLocation = (0, 2)
                                              , gTarget = curTargetP
                                              , gLastMove = STOP
                                              , gDirection = STOP
                                              }
                               }
                    else game
  where
    curTime = time game
    curTargetP = gTarget (pinky game)
    curPellets = pellets game
    dotsLeft = listLength pelletsL - listLength curPellets
    posPinky = gLocation (pinky game)


-- | Calculate distance between 2 points
distance :: Point -> Point -> Float
distance (x1,y1) (x2,y2) = sqrt ((x'*x') + (y'*y'))
    where
        x' = x2 - x1
        y' = y2 - y1

-- | Verifica se o jogador colidiu com algum dos Bosses. Diminui uma vida se acima de 0. Define gameStatus como LOST se sem vidas.
bossCollision :: MazeGame -> MazeGame
bossCollision game
    | (x', y') == (bx', by') = checkLives
    | (x', y') == (px', py') = checkLives
    | otherwise = game
  where
    (x, y) = (location (mazeGame game))
    x' = fromIntegral (round x)
    y' = fromIntegral (round y)
    ----- localizações dos Bosses
    (bx, by) = (gLocation (blinky game))
    (px, py) = (gLocation (pinky game))

    bx' = fromIntegral (round bx)
    by' = fromIntegral (round by)

    px' = fromIntegral (round px)
    py' = fromIntegral (round py)

    curLives = lives game

    checkLives = if curLives > 1
                 then game { gMode = SCATTER
                           , lcrB = (0, 0)
                           , lcrP = (0, 0)
                           , time = 0.0
                           , direction = STOP
                           , bufDirection = STOP
                           , lives = curLives - 1
                           , mazeGame = Characters { cName = MazeGame, speed = (0, 0), location = (0, -6) }
                           , pinky = Boss { gName = Pinky, gSpeed = (0, 0), gLocation = (0, 0), gTarget = pinkyScatterTarget, gLastMove = STOP, gDirection = STOP }
                           , blinky = Boss { gName = Blinky, gSpeed = (0, 0), gLocation = (0, 2), gTarget = blinkyScatterTarget, gLastMove = STOP, gDirection = STOP }
                           }
                 else game { gameStatus = LOST }

-- | Verifica se o jogador colidiu com algum dos Bosses. Diminui uma vida se acima de 0. Define gameStatus como LOST se sem vidas.
bossCollisionSwitch :: MazeGame -> MazeGame
bossCollisionSwitch game
    | mode == FRIGHTENED = case checkEaten game of
        1 -> eatBlinky
        2 -> eatPinky
        _ -> game
    | otherwise = bossCollision game
  where
    eatBlinky = game { blinky = Boss { gName = Blinky, gSpeed = (0, 0), gLocation = (bx, by), gTarget = (0, 0), gLastMove = STOP, gDirection = STOP } }
    eatPinky = game { pinky = Boss { gName = Pinky, gSpeed = (0, 0), gLocation = (px, py), gTarget = (0, 0), gLastMove = STOP, gDirection = STOP } }

    curScore = score game
    mode = gMode game
    (bx, by) = (gLocation (blinky game))
    (px, py) = (gLocation (pinky game))

-- | Verifica se o jogador colidiu com algum dos Bosses e retorna um número correspondente ao Boss.
checkEaten :: MazeGame -> Int
checkEaten game
    | (x', y') == (bx', by') = 1  -- Blinky
    | (x', y') == (px', py') = 2  -- Pinky
    | otherwise = 5
  where
    (x, y) = (location (mazeGame game))
    x' = fromIntegral (round x)
    y' = fromIntegral (round y)

    -- Localizações dos Bosses
    (bx, by) = (gLocation (blinky game))
    (px, py) = (gLocation (pinky game))

    bx' = fromIntegral (round bx)
    by' = fromIntegral (round by)

    px' = fromIntegral (round px)
    py' = fromIntegral (round py)
 
-------------------- BLINKY ------------------ 

-- | Moves Blinky to the target...which is either the top right corner or MazeGame
moveToTargetB :: MazeGame -> MazeGame
moveToTargetB game
            | curTime <= 3 = game
            | (elem (x',y') crossR) && (lastCR /= (x',y')) = game {lcrB = (x',y'), blinky = Boss {gName = Blinky, gSpeed = (0,0), gLocation = (x,y), gTarget = curTarget, gLastMove = lm, gDirection = cD}}
            | (vx,vy) == (0,0) = case bWalls of
                1 -> b1
                2 -> b2
                3 -> b3
                4 -> b4
                5 -> b5
                10 -> b10
            | (vx,vy) /= (0,0) = case (gDirection (blinky game)) of
                UP -> testUp
                DOWN -> testDown
                LEFT -> testLeft
                RIGHT -> testRight
                STOP -> stop
            | otherwise = game
            where
                (tx,ty) = (gTarget (blinky game))
                (vx,vy) = (gSpeed (blinky game))
                (x,y) = (gLocation (blinky game))
                x' = fromIntegral (round x)
                y' = fromIntegral (round y)
                yU = y' + 1
                yD = y' + (-1)
                xL = x' + (-1)
                xR = x' + 1
                lastCR = lcrB game
                curTarget = (gTarget (blinky game))
                lm = (gLastMove (blinky game))
                cD = (gDirection (blinky game))
                curTime = time game

                b1 = if lm /= UP && (distance (x',yU) (tx,ty)) < (distance (x',yD) (tx,ty)) && (distance (x',yU) (tx,ty)) < (distance (xR,y') (tx,ty)) && (distance (x',yU) (tx,ty)) < (distance (xL,y') (tx,ty))
                    then goUp
                else if lm /= DOWN && (distance (x',yD) (tx,ty)) < (distance (x',yU) (tx,ty)) && (distance (x',yD) (tx,ty)) < (distance (xR,y') (tx,ty)) && (distance (x',yD) (tx,ty)) < (distance (xL,y') (tx,ty))
                    then goDown
                else if lm /= RIGHT && (distance (xR,y') (tx,ty)) < (distance (x',yU) (tx,ty)) && (distance (xR,y') (tx,ty)) < (distance (xL,y') (tx,ty)) && (distance (xR,y') (tx,ty)) < (distance (x',yD) (tx,ty))
                    then goRight
                else if lm /= LEFT && (distance (xL,y') (tx,ty)) < (distance (x',yU) (tx,ty)) && (distance (xL,y') (tx,ty)) < (distance (xR,y') (tx,ty)) && (distance (xL,y') (tx,ty)) < (distance (x',yD) (tx,ty))
                    then goLeft
                else if lm /= RIGHT && (checkInList (xR, y') coords) then goRight else if lm /= UP && (checkInList (x', yU) coords) then goUp else if lm /= LEFT && (checkInList (xL, y') coords) then goLeft else if lm /= DOWN && (checkInList (x', yD) coords) then goDown
                else game

                b2 = if lm /= UP && (distance (x',yU) (tx,ty)) <= (distance (xR,y') (tx,ty)) && (distance (x',yU) (tx,ty)) <= (distance (xL,y') (tx,ty))
                    then goUp
                else if lm /= RIGHT && (distance (xR,y') (tx,ty)) <= (distance (x',yU) (tx,ty)) && (distance (xR,y') (tx,ty)) <= (distance (xL,y') (tx,ty))
                    then goRight
                else if lm /= LEFT && (distance (xL,y') (tx,ty)) <= (distance (x',yU) (tx,ty)) && (distance (xL,y') (tx,ty)) <= (distance (xR,y') (tx,ty))
                    then goLeft
                else if lm /= UP && (checkInList (x', yU) coords) then goUp else if lm /= RIGHT && (checkInList (xR, y') coords) then goRight else if lm /= LEFT && (checkInList (xL, y') coords) then goLeft else if lm /= DOWN && (checkInList (x', yD) coords) then goDown
                else game

                b3 = if lm /= UP && (distance (x',yU) (tx,ty)) < (distance (x',yD) (tx,ty)) && (distance (x',yU) (tx,ty)) < (distance (xR,y') (tx,ty))
                    then goUp
                else if lm /= DOWN && (distance (x',yD) (tx,ty)) < (distance (x',yU) (tx,ty)) && (distance (x',yD) (tx,ty)) < (distance (xR,y') (tx,ty))
                    then goDown
                else if lm /= RIGHT && (distance (xR,y') (tx,ty)) < (distance (x',yU) (tx,ty)) && (distance (xR,y') (tx,ty)) < (distance (x',yD) (tx,ty))
                    then goRight
                else if lm /= RIGHT && (checkInList (xR, y') coords) then goRight else if lm /= UP && (checkInList (x', yU) coords) then goUp else if lm /= LEFT && (checkInList (xL, y') coords) then goLeft else if lm /= DOWN && (checkInList (x', yD) coords) then goDown
                else game

                b4 = if lm /= DOWN && (distance (x',yD) (tx,ty)) < (distance (xR,y') (tx,ty)) && (distance (x',yD) (tx,ty)) < (distance (xL,y') (tx,ty))
                    then goDown
                else if lm /= RIGHT && (distance (xR,y') (tx,ty)) < (distance (xL,y') (tx,ty)) && (distance (xR,y') (tx,ty)) < (distance (x',yD) (tx,ty))
                    then goRight
                else if lm /= LEFT && (distance (xL,y') (tx,ty)) < (distance (xR,y') (tx,ty)) && (distance (xL,y') (tx,ty)) < (distance (x',yD) (tx,ty))
                    then goLeft
                else if lm /= RIGHT && (checkInList (xR, y') coords) then goRight else if lm /= UP && (checkInList (x', yU) coords) then goUp else if lm /= LEFT && (checkInList (xL, y') coords) then goLeft else if lm /= DOWN && (checkInList (x', yD) coords) then goDown
                else game

                b5 = if lm /= UP && (distance (x',yU) (tx,ty)) < (distance (x',yD) (tx,ty)) && (distance (x',yU) (tx,ty)) < (distance (xL,y') (tx,ty))
                    then goUp
                else if lm /= DOWN && (distance (x',yD) (tx,ty)) < (distance (x',yU) (tx,ty)) && (distance (x',yD) (tx,ty)) < (distance (xL,y') (tx,ty))
                    then goDown
                else if lm /= LEFT && (distance (xL,y') (tx,ty)) < (distance (x',yU) (tx,ty)) && (distance (xL,y') (tx,ty)) < (distance (x',yD) (tx,ty))
                    then goLeft
                else if lm /= RIGHT && (checkInList (xR, y') coords) then goRight else if lm /= UP && (checkInList (x', yU) coords) then goUp else if lm /= LEFT && (checkInList (xL, y') coords) then goLeft else if lm /= DOWN && (checkInList (x', yD) coords) then goDown
                else game

                b10 = if lm /= RIGHT && (checkInList (xR, y') coords) then goRight else if lm /= UP && (checkInList (x', yU) coords) then goUp else if lm /= LEFT && (checkInList (xL, y') coords) then goLeft else if lm /= DOWN && (checkInList (x', yD) coords)
                    then goDown
                else game 

                 -- Determines where the walls are for a specific Boss location
                bWalls = findWalls game 1

                -- Movement
                goRight = game {blinky = Boss {gName = Blinky, gSpeed = (3.7,0), gLocation = (x',y'), gTarget = curTarget, gLastMove = LEFT, gDirection = RIGHT}}
                goLeft = game {blinky = Boss {gName = Blinky, gSpeed = (-3.7,0), gLocation = (x',y'), gTarget = curTarget, gLastMove = RIGHT, gDirection = LEFT}}
                goUp = game {blinky = Boss {gName = Blinky, gSpeed = (0,3.7), gLocation = (x',y'), gTarget = curTarget, gLastMove = DOWN, gDirection = UP}}
                goDown = game {blinky = Boss {gName = Blinky, gSpeed = (0,-3.7), gLocation = (x',y'), gTarget = curTarget, gLastMove = UP, gDirection = DOWN}}

                -- When velocity isn't at 0, check collision of wall.
                testUp = if (elem (x',yU) coords) then game {blinky = Boss {gName = Blinky, gSpeed = (0,0), gLocation = (x',y'), gTarget = curTarget, gLastMove = lm, gDirection = cD}} else game
                testDown = if (elem (x',yD) coords) then game {blinky = Boss {gName = Blinky, gSpeed = (0,0), gLocation = (x',y'), gTarget = curTarget, gLastMove = lm, gDirection = cD}} else game
                testLeft = if (elem (xL,y') coords) then game {blinky = Boss {gName = Blinky, gSpeed = (0,0), gLocation = (x',y'), gTarget = curTarget, gLastMove = lm, gDirection = cD}} else game
                testRight = if (elem (xR,y') coords) then game {blinky = Boss {gName = Blinky, gSpeed = (0,0), gLocation = (x',y'), gTarget = curTarget, gLastMove = lm, gDirection = cD}} else game
                stop = game {blinky = Boss {gName = Blinky, gSpeed = (0,0), gLocation = (x',y'), gTarget = curTarget, gLastMove = lm, gDirection = cD}}


-------------------- PINKY ------------------

-- | Moves Pinky to the target...which is either the top left corner or a few spaces in front of MazeGame
moveToTargetP :: MazeGame -> MazeGame
moveToTargetP game
            | curTime <= 4 = game
            | (elem (x',y') crossR) && (lastCR /= (x',y')) = game {lcrP = (x',y'), pinky = Boss {gName = Pinky, gSpeed = (0,0), gLocation = (x,y), gTarget = (curTarget), gLastMove = lm, gDirection = cD}}
            | (vx,vy) == (0,0) = case pWalls of
                1 -> p1
                2 -> p2
                3 -> p3
                4 -> p4
                5 -> p5
                10 -> p10
            | (vx,vy) /= (0,0) = case (gDirection (pinky game)) of
                UP -> testUp
                DOWN -> testDown
                LEFT -> testLeft
                RIGHT -> testRight
                STOP -> stop
            | otherwise = game
            where
                (tx,ty) = (gTarget (pinky game))
                (vx,vy) = (gSpeed (pinky game))
                (x,y) = (gLocation (pinky game))
                x' = fromIntegral (round x)
                y' = fromIntegral (round y)
                yU = y' + 1
                yD = y' + (-1)
                xL = x' + (-1)
                xR = x' + 1
                lastCR = lcrP game
                curTarget = (gTarget (pinky game))
                lm = (gLastMove (pinky game))
                cD = (gDirection (pinky game))
                curTime = time game

                -- Determines where the walls are for a specific Boss location
                pWalls = findWalls game 2


                p1 = if lm /= UP && (distance (x',yU) (tx,ty)) < (distance (x',yD) (tx,ty)) && (distance (x',yU) (tx,ty)) < (distance (xR,y') (tx,ty)) && (distance (x',yU) (tx,ty)) < (distance (xL,y') (tx,ty))
                    then goUp
                else if lm /= DOWN && (distance (x',yD) (tx,ty)) < (distance (x',yU) (tx,ty)) && (distance (x',yD) (tx,ty)) < (distance (xR,y') (tx,ty)) && (distance (x',yD) (tx,ty)) < (distance (xL,y') (tx,ty))
                    then goDown
                else if lm /= RIGHT && (distance (xR,y') (tx,ty)) < (distance (x',yU) (tx,ty)) && (distance (xR,y') (tx,ty)) < (distance (xL,y') (tx,ty)) && (distance (xR,y') (tx,ty)) < (distance (x',yD) (tx,ty))
                    then goRight
                else if lm /= LEFT && (distance (xL,y') (tx,ty)) < (distance (x',yU) (tx,ty)) && (distance (xL,y') (tx,ty)) < (distance (xR,y') (tx,ty)) && (distance (xL,y') (tx,ty)) < (distance (x',yD) (tx,ty))
                    then goLeft
                else if lm /= RIGHT && (checkInList (xR, y') coords) then goRight else if lm /= UP && (checkInList (x', yU) coords) then goUp else if lm /= LEFT && (checkInList (xL, y') coords) then goLeft else if lm /= DOWN && (checkInList (x', yD) coords) then goDown
                else game

                p2 = if lm /= UP && (distance (x',yU) (tx,ty)) <= (distance (xR,y') (tx,ty)) && (distance (x',yU) (tx,ty)) <= (distance (xL,y') (tx,ty))
                    then goUp
                else if lm /= RIGHT && (distance (xR,y') (tx,ty)) <= (distance (x',yU) (tx,ty)) && (distance (xR,y') (tx,ty)) <= (distance (xL,y') (tx,ty))
                    then goRight
                else if lm /= LEFT && (distance (xL,y') (tx,ty)) <= (distance (x',yU) (tx,ty)) && (distance (xL,y') (tx,ty)) <= (distance (xR,y') (tx,ty))
                    then goLeft
                else if lm /= UP && (checkInList (x', yU) coords) then goUp else if lm /= RIGHT && (checkInList (xR, y') coords) then goRight else if lm /= LEFT && (checkInList (xL, y') coords) then goLeft else if lm /= DOWN && (checkInList (x', yD) coords) then goDown
                else game

                p3 = if lm /= UP && (distance (x',yU) (tx,ty)) < (distance (x',yD) (tx,ty)) && (distance (x',yU) (tx,ty)) < (distance (xR,y') (tx,ty))
                    then goUp
                else if lm /= DOWN && (distance (x',yD) (tx,ty)) < (distance (x',yU) (tx,ty)) && (distance (x',yD) (tx,ty)) < (distance (xR,y') (tx,ty))
                    then goDown
                else if lm /= RIGHT && (distance (xR,y') (tx,ty)) < (distance (x',yU) (tx,ty)) && (distance (xR,y') (tx,ty)) < (distance (x',yD) (tx,ty))
                    then goRight
                else if lm /= RIGHT && (checkInList (xR, y') coords) then goRight else if lm /= UP && (checkInList (x', yU) coords) then goUp else if lm /= LEFT && (checkInList (xL, y') coords) then goLeft else if lm /= DOWN && (checkInList (x', yD) coords) then goDown
                else game

                p4 = if lm /= DOWN && (distance (x',yD) (tx,ty)) < (distance (xR,y') (tx,ty)) && (distance (x',yD) (tx,ty)) < (distance (xL,y') (tx,ty))
                    then goDown
                else if lm /= RIGHT && (distance (xR,y') (tx,ty)) < (distance (xL,y') (tx,ty)) && (distance (xR,y') (tx,ty)) < (distance (x',yD) (tx,ty))
                    then goRight
                else if lm /= LEFT && (distance (xL,y') (tx,ty)) < (distance (xR,y') (tx,ty)) && (distance (xL,y') (tx,ty)) < (distance (x',yD) (tx,ty))
                    then goLeft
                else if lm /= RIGHT && (checkInList (xR, y') coords) then goRight else if lm /= UP && (checkInList (x', yU) coords) then goUp else if lm /= LEFT && (checkInList (xL, y') coords) then goLeft else if lm /= DOWN && (checkInList (x', yD) coords) then goDown
                else game

                p5 = if lm /= UP && (distance (x',yU) (tx,ty)) < (distance (x',yD) (tx,ty)) && (distance (x',yU) (tx,ty)) < (distance (xL,y') (tx,ty))
                    then goUp
                else if lm /= DOWN && (distance (x',yD) (tx,ty)) < (distance (x',yU) (tx,ty)) && (distance (x',yD) (tx,ty)) < (distance (xL,y') (tx,ty))
                    then goDown
                else if lm /= LEFT && (distance (xL,y') (tx,ty)) < (distance (x',yU) (tx,ty)) && (distance (xL,y') (tx,ty)) < (distance (x',yD) (tx,ty))
                    then goLeft
                else if lm /= RIGHT && (checkInList (xR, y') coords) then goRight else if lm /= UP && (checkInList (x', yU) coords) then goUp else if lm /= LEFT && (checkInList (xL, y') coords) then goLeft else if lm /= DOWN && (checkInList (x', yD) coords) then goDown
                else game

                p10 = if lm /= RIGHT && (checkInList (xR, y') coords) then goRight else if lm /= UP && (checkInList (x', yU) coords) then goUp else if lm /= LEFT && (checkInList (xL, y') coords) then goLeft else if lm /= DOWN && (checkInList (x', yD) coords)
                    then goDown
                else game 


                -- Movement
                goRight = game {pinky = Boss {gName = Pinky, gSpeed = (3.7,0), gLocation = (x',y'), gTarget = curTarget, gLastMove = LEFT, gDirection = RIGHT}}
                goLeft = game {pinky = Boss {gName = Pinky, gSpeed = (-3.7,0), gLocation = (x',y'), gTarget = curTarget, gLastMove = RIGHT, gDirection = LEFT}}
                goUp = game {pinky = Boss {gName = Pinky, gSpeed = (0,3.7), gLocation = (x',y'), gTarget = curTarget, gLastMove = DOWN, gDirection = UP}}
                goDown = game {pinky = Boss {gName = Pinky, gSpeed = (0,-3.7), gLocation = (x',y'), gTarget = curTarget, gLastMove = UP, gDirection = DOWN}}

                -- When velocity isn't at 0, check collision of wall.
                testUp = if (elem (x',yU) coords) then game {pinky = Boss {gName = Pinky, gSpeed = (0,0), gLocation = (x',y'), gTarget = curTarget, gLastMove = lm, gDirection = cD}} else game
                testDown = if (elem (x',yD) coords) then game {pinky = Boss {gName = Pinky, gSpeed = (0,0), gLocation = (x',y'), gTarget = curTarget, gLastMove = lm, gDirection = cD}} else game
                testLeft = if (elem (xL,y') coords) then game {pinky = Boss {gName = Pinky, gSpeed = (0,0), gLocation = (x',y'), gTarget = curTarget, gLastMove = lm, gDirection = cD}} else game
                testRight = if (elem (xR,y') coords) then game {pinky = Boss {gName = Pinky, gSpeed = (0,0), gLocation = (x',y'), gTarget = curTarget, gLastMove = lm, gDirection = cD}} else game
                stop = game {pinky = Boss {gName = Pinky, gSpeed = (0,0), gLocation = (x',y'), gTarget = curTarget, gLastMove = lm, gDirection = cD}}

-- Represents where walls are in relative to position
-- 1 = all 4 sides
-- 2 = Left, up, right
-- 3 = up, right, down
-- 4 = right, down, left
-- 5 = down, left, up
-- 6 = up, right
-- 7 = right, down
-- 8 = down, left
-- 9 = left up
-- | Used for chase AI, Checks current position with walls, returning a number which represents how and where the walls are.
findWalls :: MazeGame -> Int -> Int
findWalls game x = case x of 
                1 -> checkBlinky
                2 -> checkPinky
                where
                    --- Blinky ---
                    (bx,by) = (gLocation (blinky game))
                    bx' = fromIntegral (round bx)
                    by' = fromIntegral (round by)
                    bU = by' + 1
                    bD = by' + (-1)
                    bL = bx' + (-1)
                    bR = bx' + (1)
                    checkBlinky =
                        if (checkInList (bL, by') coords) && (checkInList (bR, by') coords) && (checkInList (bx', bU) coords) && (checkInList (bx', bD) coords) then 1
                        else if (checkInList (bL, by') coords) && (checkInList (bR, by') coords) && (checkInList (bx', bU) coords) && (elem (bx', bD) coords) then 2
                        else if (elem (bL, by') coords) && (checkInList (bR, by') coords) && (checkInList (bx', bU) coords) && (checkInList (bx', bD) coords) then 3
                        else if (checkInList (bL, by') coords) && (checkInList (bR, by') coords) && (elem (bx', bU) coords) && (checkInList (bx', bD) coords) then 4
                        else if (checkInList (bL, by') coords) && (elem (bR, by') coords) && (checkInList (bx', bU) coords) && (checkInList (bx', bD) coords) then 5
                        else 10
                            
                    --- Pinky ---
                    (px,py) = (gLocation (pinky game))
                    px' = fromIntegral (round px)
                    py' = fromIntegral (round py)
                    pU = py' + 1
                    pD = py' + (-1)
                    pL = px' + (-1)
                    pR = px' + (1)
                    checkPinky = 
                        if (checkInList (pL, py') coords) && (checkInList (pR, py') coords) && (checkInList (px', pU) coords) && (checkInList (px', pD) coords) then 1
                        else if (checkInList (pL, py') coords) && (checkInList (pR, py') coords) && (checkInList (px', pU) coords) && (elem (px', pD) coords) then 2
                        else if (elem (pL, py') coords) && (checkInList (pR, py') coords) && (checkInList (px', pU) coords) && (checkInList (px', pD) coords) then 3
                        else if (checkInList (pL, py') coords) && (checkInList (pR, py') coords) && (elem (px', pU) coords) && (checkInList (px', pD) coords) then 4
                        else if (checkInList (pL, py') coords) && (elem (pR, py') coords) && (checkInList (px', pU) coords) && (checkInList (px', pD) coords) then 5
                        else 10