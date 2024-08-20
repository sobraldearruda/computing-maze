module Game (displayWindow, initialize, render, handleKeys, beginTimer, sMove, pacScore, pacEat, pacEatP, wallCollision, checkTeleport, execM) where

    import Graphics.Gloss
    import Graphics.Gloss.Interface.Pure.Game
    import Graphics.Gloss.Data.Point
    import Data.Fixed

    import Maze
    import Utils
    import BossAI (listLength)


    -- | Lidar com entradas do usuário. Necessário para a função de reprodução do gloss
    handleKeys :: Event -> MazeGame -> MazeGame
    handleKeys event game
            | curTime < 3 = game
            | otherwise =  case event of
                EventKey (SpecialKey KeyUp) _ _ _ -> game { bufDirection = UP }
                EventKey (SpecialKey KeyDown) _ _ _ -> game { bufDirection = DOWN }
                EventKey (SpecialKey KeyLeft) _ _ _ -> game { bufDirection = LEFT }
                EventKey (SpecialKey KeyRight) _ _ _ -> game { bufDirection = RIGHT }
                EventKey (Char 'w') _ _ _ -> game { bufDirection = UP }
                EventKey (Char 's') _ _ _ -> game { bufDirection = DOWN }
                EventKey (Char 'a') _ _ _ -> game { bufDirection = LEFT }
                EventKey (Char 'd') _ _ _ -> game { bufDirection = RIGHT }
                _ -> game
            where
                curTime = time game
        

    -- | Inicialize o estado base do jogo
    initialize :: MazeGame 
    initialize = Game
        { lives = 3
        , gameStatus = PLAYING
        , mazeGame = Characters {cName = MazeGame, speed = (0,0), location = (0,-6)}
        , pinky = Boss {gName = Pinky, gSpeed = (0,0), gLocation = (0,0), gTarget = pinkyScatterTarget, gLastMove = STOP, gDirection = STOP}
        , blinky = Boss {gName = Blinky, gSpeed = (0,0), gLocation = (0,2), gTarget = blinkyScatterTarget, gLastMove = STOP, gDirection = STOP}
        , time = 0.0
        , score = 0
        , pellets = pelletsL
        , direction = LEFT
        , bufDirection = STOP
        , gMode = SCATTER
        , lcrB = (0,0)
        , lcrP = (0,0)
        , pPellets = powerPellet
        , fTime = 0.0
        }

    -- | Temporizador que avisa quando liberar os Bosses no labirinto.
    beginTimer :: MazeGame -> MazeGame
    beginTimer game = game {time = curTime+1/(fromIntegral fps)}
        where
            curTime = time game

    -- | Verifica se o ponto dado está dentro dos pontos que criam as paredes do labirinto
    checkCollision :: Point -> Bool
    checkCollision c = (c `elem` coords)

    -- | Renderiza a imagem do MazeGame com base no gamestate.
    renderMazeGame :: MazeGame -> Picture
    renderMazeGame game =
        translate (x*30) (y*30) $ color yellow $ circleSolid 13
            where 
                (x, y) = (location $ mazeGame game)

    -- | Renderiza a imagem de Pinky com base no gamestate.
    renderPinky :: MazeGame -> Picture
    renderPinky game
                | mode == CHASE || mode == SCATTER = translate (x*30) (y*30) $ color (light rose) $ circleSolid 13
                | target == (0,0) = translate (x*30) (y*30) $ color (dark white) $ circleSolid 13
                | otherwise = translate (x*30) (y*30) $ color blue $ circleSolid 13
            where 
                (x, y) = (gLocation $ pinky game)
                mode = gMode game
                target = gTarget (pinky game)
    
    -- | Renderiza a imagem do Blinky com base no gamestate.
    renderBlinky :: MazeGame -> Picture
    renderBlinky game
                | mode == CHASE || mode == SCATTER = translate (x*30) (y*30) $ color red $ circleSolid 13
                | target == (0,0) = translate (x*30) (y*30) $ color (dark white) $ circleSolid 13
                | otherwise = translate (x*30) (y*30) $ color blue $ circleSolid 13
                where 
                    (x, y) = (gLocation $ blinky game)
                    mode = gMode game
                    target = gTarget (blinky game)

    -- | Desenha a interface do jogo, exibindo o "Ready!" nos primeiros 3 segundos e, depois, mostrando a pontuação, vidas e o modo do jogo. 
    renderStuff :: MazeGame -> Picture
    renderStuff game
                | curTime <= 3 = translate (-30) (-65) $ scale 0.15 0.15 $ color yellow $ Text "Ready!"
                | otherwise = color white $ scale 0.2 0.2 $ pictures
                    [rScore, rLives, mode] 
                    where
                        rScore = translate (-1400) (1475) $ Text $ "Score: " ++ show (score game)
                        rLives = translate (-1800) (-1880) $ Text $ "Lives: "
                        mode = translate (200) (1475) $ Text $ "Mode: " ++ show (gMode game)
                        curTime = time game

    -- | Renderiza as imagens dos pellets com base no estado do jogo.
    renderPellets :: MazeGame -> Picture
    renderPellets game = pictures $ [translate (x*30) (y*30) $ color white $ circleSolid 4 | (x,y) <- pellets game]

    -- | Renderiza as imagens do power pellet com base no estado do jogo.
    renderPowerPellets :: MazeGame -> Picture
    renderPowerPellets game
                        | mod' curTime 0.3 >= 0 && mod' curTime 0.3 <= 0.15 = pictures $ [translate (x*30) (y*30) $ color white $ circleSolid 8 | (x,y) <- pPellets game]
                        | otherwise = Blank
                        where
                            curTime = time game

    -- | Render the lives on the bottom of the screen
    renderLives :: MazeGame -> Picture
    renderLives game
                | curTime <= 3 = Blank
                | curLives >= 3 = pictures $ [renderOne, renderTwo, renderThree]
                | curLives == 2 = pictures $ [renderOne, renderTwo]
                | curLives == 1 = pictures $ [renderOne]
                | otherwise = Blank
                where
                    curTime = time game
                    curLives = lives game
                    renderOne = translate (-270) (-370) $ color yellow $ circleSolid 13
                    renderTwo = translate (-235) (-370) $ color yellow $ circleSolid 13
                    renderThree = translate (-200) (-370) $ color yellow $ circleSolid 13

    -- | Main display window.
    displayWindow :: Display
    displayWindow = InWindow "computing-maze" (800,800) (350, 350)

    -- | Renders the victory screen
    renderVictory :: Picture
    renderVictory = translate (-250) (0) $ scale 0.3 0.3 $ color white $ Text "Congratulations! You Win!"

    -- | Renders the defeat screen
    renderDefeat :: Picture
    renderDefeat = translate (-170) (0) $ scale 0.4 0.4 $ color white $ Text "Game Over"

    -- | Main render function used in play. List of multiple other render functions that layer on one another.
    render :: MazeGame -> Picture
    render game = case gameStatus game of
        WON -> renderVictory
        LOST -> renderDefeat
        PLAYING -> pictures $ [(renderLives game),(renderPowerPellets game),(renderW coords),(renderMazeGame game),(renderStuff game),(renderPellets game),(renderPinky game),(renderBlinky game)]

-- | This is the standard movement function. Take the velocity and position, then update it with (position + velocity * time)
    sMove :: Float -> MazeGame -> MazeGame
    sMove sec game = game {mazeGame = Characters {cName = MazeGame, location = (x', y'), speed = (vx,vy)}, 
                            blinky = Boss {gName = Blinky, gLocation = (bx',by'), gSpeed = (bvx,bvy), gTarget = curTargetB, gLastMove = curLMB, gDirection = curDirectionB},
                            pinky = Boss {gName = Pinky, gLocation = (px',py'), gSpeed = (pvx,pvy), gTarget = curTargetP, gLastMove = curLMP, gDirection = curDirectionP}
                            }
        where
            (x, y) = (location $ mazeGame game)
            (vx, vy) = (speed $ mazeGame game)
            x' = x + vx * sec
            y' = y + vy * sec

            ------ Blinky
            (bx, by) = (gLocation (blinky game))
            (bvx, bvy) = (gSpeed (blinky game))
            bx' = bx + bvx * sec
            by' = by + bvy * sec
            curTargetB = (gTarget (blinky game))
            curLMB = (gLastMove (blinky game))
            curDirectionB = (gDirection (blinky game))

            ------ Pinky
            (px, py) = (gLocation (pinky game))
            (pvx, pvy) = (gSpeed (pinky game))
            px' = px + pvx * sec
            py' = py + pvy * sec
            curTargetP = (gTarget (pinky game))
            curLMP = (gLastMove (pinky game))
            curDirectionP = (gDirection (pinky game))
        
    -- | Returns true if mazeGame is stopped.
    notMoving :: MazeGame -> Bool
    notMoving game = if direction game == STOP then True else False


    -- | Function that handles mazeGames movements
    execM :: MazeGame -> MazeGame
    execM game
        | curTime <= 2.5 = game
        | direction game == bufDirection game = game                -- If the buffer direction and the game direction are equal, keep going
        | direction game /= bufDirection game = newgame             -- If the buffer direction and the game direction are not euqal, use newgame
        | otherwise = game                                          -- This shouldn't trigger, but there for compiling sake
        where
            curTime = time game
            newgame 
                | (vx, vy) == (0,0) = case bufDirection game of     -- Check the cases of the buffer if the mazeGame is stopped.
                    UP -> up                                        -- Will move the mazeGame UP
                    DOWN -> down                                    -- Will move the mazeGame DOWN
                    LEFT -> left                                    -- Will move the mazeGame LEFT
                    RIGHT -> right                                  -- Will move the mazeGame RIGHT
                    STOP -> stop                                    -- Will stop the mazeGame
                | (vx, vy) /= (0, 0) = case bufDirection game of    -- Case where the mazeGame is moving but the buffer is different. Makes sure the mazeGame can change directions when moving into other paths
                    UP -> tUP                                       -- Will change direction to UP
                    DOWN -> tDOWN                                   -- Will change direction to DOWN
                    LEFT -> tLEFT                                   -- Will change direction to LEFT
                    RIGHT -> tRIGHT                                 -- Will change direction to RIGHT
                    STOP -> tSTOP                                   -- Will stop the mazeGame
                | otherwise = game
                where
                    (x,y) = (location (mazeGame game))
                    (vx, vy) = (speed (mazeGame game))
                    up = game {direction = UP, mazeGame = Characters {cName = MazeGame, speed = (0,4), location = (x,y)}}
                    down = game {direction = DOWN, mazeGame = Characters {cName = MazeGame, speed = (0,-4), location = (x,y)}}
                    left = game {direction = LEFT, mazeGame = Characters {cName = MazeGame, speed = (-4,0), location = (x,y)}}
                    right = game {direction = RIGHT, mazeGame = Characters {cName = MazeGame, speed = (4,0), location = (x,y)}}
                    stop = game {direction = STOP, mazeGame = Characters {cName = MazeGame, speed = (0,0), location = (x,y)}}
                    x' = fromIntegral (round x)
                    y' = fromIntegral (round y)
                    tUP = if (elem (x', (y'+1)) coords) then game else game {direction = UP, mazeGame = Characters {cName = MazeGame, speed = (0,4), location = (x',y')}}
                    tDOWN = if (elem (x', (y'-1)) coords) then game else game {direction = DOWN, mazeGame = Characters {cName = MazeGame, speed = (0,-4), location = (x',y')}}
                    tLEFT = if (elem ((x'+(-1)), y') coords) then game else game {direction = LEFT, mazeGame = Characters {cName = MazeGame, speed = (-4,0), location = (x',y')}}
                    tRIGHT = if (elem ((x'+1), y') coords) then game else game {direction = RIGHT, mazeGame = Characters {cName = MazeGame, speed = (4,0), location = (x',y')}}
                    tSTOP = game


    -- | Verificações de colisão em paredes
    wallCollision :: MazeGame -> MazeGame
    wallCollision game = case direction game of         -- Checks cases for the current moving direction
        UP -> up                                        -- Checks if position above is a wall. If not then move UP
        DOWN -> down                                    -- Checks if position below is a wall. If not then move DOWN
        LEFT -> left                                    -- Checks if position left is a wall. If not then move LEFT
        RIGHT -> right                                  -- Checks if position right is a wall. If not then move RIGHTs
        STOP -> stop                                    -- stops the mazeGame
        where
            (x,y) = (location (mazeGame game))
            (vx, vy) = (speed (mazeGame game))
            x' = fromIntegral (round x)
            y' = fromIntegral (round y)
            yU = y' + 1
            yD = y' + (-1)
            xL = x' + (-1)
            xR = x' + 1
            up = if (elem (x',yU) coords) then game {direction = STOP, mazeGame = Characters {cName = MazeGame, speed = (0,0), location = (x',y')}} else game
            down = if (elem (x',yD) coords) then game {direction = STOP, mazeGame = Characters {cName = MazeGame, speed = (0,0), location = (x',y')}} else game
            left = if (elem (xL,y') coords) then game {direction = STOP, mazeGame = Characters {cName = MazeGame, speed = (0,0), location = (x',y')}} else game
            right = if (elem (xR,y') coords) then game {direction = STOP, mazeGame = Characters {cName = MazeGame, speed = (0,0), location = (x',y')}} else game
            stop = game {direction = STOP, mazeGame = Characters {cName = MazeGame, speed = (0,0), location = (x',y')}}


    -- | Função que verifica se o jogador está acima de uma bolinha. Se sim, remova-o da lista mantida no gamestate
    pacEat :: MazeGame -> MazeGame
    pacEat game
            | (x',y') `elem` pelletsL = game {pellets = [x | x <- curPellets, x /= (x',y')]}
            | otherwise = game
            where
                (x,y) = (location (mazeGame game))
                x' = fromIntegral (round x)
                y' = fromIntegral (round y)
                curPellets = pellets game
                curScore = (score game)

    -- | Função que verifica se o player está sobre um power pellet. Se sim, remova-o da lista mantida no estado do jogo e mude o modo para assustado
    pacEatP :: MazeGame -> MazeGame
    pacEatP game
            | (x',y') `elem` pPelletsL = game {fTime = 0, gMode = FRIGHTENED, pPellets = [x | x <- curPellets, x /= (x',y')]}
            | otherwise = game
            where
                (x,y) = (location (mazeGame game))
                x' = fromIntegral (round x)
                y' = fromIntegral (round y)
                curPellets = pPellets game
                curScore = (score game)
                pPelletsL = pPellets game



    
    -- | Determina a pontuação atual do seu jogo. Pelotas normais valem 10 pontos. Pelotas Grandes valem 50
    pacScore :: MazeGame -> MazeGame
    pacScore game 
                | curScore >= 1700 = game {gameStatus = WON}
                | otherwise = game {score = newscore}
                where
                    newscore = (((listLength pelletsL) - (listLength curPellets))*10) + (((listLength powerPellet) - (listLength curPowerPellets))*50)
                    curPellets = pellets game
                    curScore = score game
                    curPowerPellets = pPellets game


    -- | Verifique se o jogador está no local de teletransporte. Se sim, teletransporte-o para o outro lado
    checkTeleport :: MazeGame -> MazeGame
    checkTeleport game
                    | (x',y') == (-10,0) = game {direction = curDirection, mazeGame = Characters {cName = MazeGame, speed = curSpeed, location = (9,0)}}
                    | (x',y') == (10,0) = game {direction = curDirection, mazeGame = Characters {cName = MazeGame, speed = curSpeed, location = (-9,0)}}
                    | otherwise = game
                    where
                        (x,y) = (location (mazeGame game))
                        x' = fromIntegral (round x)
                        y' = fromIntegral (round y)
                        curSpeed = (speed (mazeGame game))
                        curDirection = direction game
