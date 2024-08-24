module Game (displayWindow, initialize, render, handleKeys, beginTimer, sMove, mazeScore, mazeEat, mazeEatP, wallCollision, checkTeleport, execM) where

    import Graphics.Gloss
    import Graphics.Gloss.Interface.Pure.Game
    import Graphics.Gloss.Data.Point
    import Data.Fixed

    import Maze
    import Utils
    import BossIA (listLength)


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
    displayWindow = InWindow "Computing-Maze" (800,800) (350, 350)

    -- | Renders the victory screen
    renderVictory :: Picture
    renderVictory = translate (-250) (0) $ scale 0.3 0.3 $ color white $ Text "Congratulations! You Win!"

    -- | Renders the defeat screen
    renderDefeat :: Picture
    renderDefeat = translate (-170) (0) $ scale 0.4 0.4 $ color white $ Text "Game Over"

    -- | Função de renderização principal usada no jogo. Lista de várias outras funções de renderização que se sobrepõem.
    render :: MazeGame -> Picture
    render game = case gameStatus game of
        WON -> renderVictory
        LOST -> renderDefeat
        PLAYING -> pictures $ [(renderLives game),(renderPowerPellets game),(renderW coords),(renderMazeGame game),(renderStuff game),(renderPellets game),(renderPinky game),(renderBlinky game)]

    -- | Esta é a função de movimento padrão. Pegue a velocidade e a posição, e atualize-as com (posição + velocidade * tempo)
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
        
    -- | Retorna verdadeiro se mazeGame for interrompido.
    notMoving :: MazeGame -> Bool
    notMoving game = if direction game == STOP then True else False

    -- | Função que lida com os movimentos do mazeGames
    execM :: MazeGame -> MazeGame
    execM game
        | curTime <= 2.5 = game
        | direction game == bufDirection game = game                -- Se a direção do buffer e a direção do jogo forem iguais, continue
        | direction game /= bufDirection game = newgame             -- Se a direção do buffer e a direção do jogo não forem iguais, use um novo jogo
        | otherwise = game                                          -- Isso não deveria ser acionado, mas está aí para fins de compilação
        where
            curTime = time game
            newgame 
                | (vx, vy) == (0,0) = case bufDirection game of     -- Verifique os casos do buffer se o mazeGame estiver parado.
                    UP -> up                                        -- Moverá o labirinto para CIMA
                    DOWN -> down                                    -- Moverá o labirinto para BAIXO
                    LEFT -> left                                    -- Moverá o labirinto para ESQUERDA
                    RIGHT -> right                                  -- Moverá o labirinto para DIREITA
                    STOP -> stop                                    -- Vai parar o JOGO 
                | (vx, vy) /= (0, 0) = case bufDirection game of    -- Caso em que o player está se movendo, mas o buffer é diferente. Garante que o player possa mudar de direção ao se mover para outros caminhos
                    UP -> tUP                                       -- Mudará de direção para CIMA
                    DOWN -> tDOWN                                   -- Mudará de direção para BAIXO
                    LEFT -> tLEFT                                   -- Mudará de direção para ESQUERDA
                    RIGHT -> tRIGHT                                 -- Mudará de direção para DIREITA
                    STOP -> tSTOP                                   -- Vai parar o JOGO
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
    wallCollision game = case direction game of         -- Verifica casos para a direção atual do movimento
        UP -> up                                        -- Verifica se a posição acima é uma parede. Se não, então mova PARA CIMA
        DOWN -> down                                    -- Verifica se a posição abaixo é uma parede. Se não, então mova PARA BAIXO
        LEFT -> left                                    -- Verifica se a posição esquerda é uma parede. Se não, mova para a ESQUERDA
        RIGHT -> right                                  -- Verifica se a posição direita é uma parede. Se não, mova para a DIREITA
        STOP -> stop                                    -- Para o jogo
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

    -- | Função que verifica se o jogador está acima de uma bolinha. Se sim, remova-o da lista mantida no estado do jogo
    mazeEat :: MazeGame -> MazeGame
    mazeEat game
            | (x',y') `elem` pelletsL = game {pellets = [x | x <- curPellets, x /= (x',y')]}
            | otherwise = game
            where
                (x,y) = (location (mazeGame game))
                x' = fromIntegral (round x)
                y' = fromIntegral (round y)
                curPellets = pellets game
                curScore = (score game)

    -- | Função que verifica se o player está sobre um power pellet. Se sim, remova-o da lista mantida no estado do jogo e mude o modo para assustado
    mazeEatP :: MazeGame -> MazeGame
    mazeEatP game
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
    mazeScore :: MazeGame -> MazeGame
    mazeScore game 
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
