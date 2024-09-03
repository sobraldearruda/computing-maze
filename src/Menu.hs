module Menu (
    MenuState (..),
    renderMenu,
    menuEventHandler,
    menuTimeHandler
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- Tipo de dado 'MenuState', que representa os diferentes estados possíveis do menu:
-- 'MainMenu', 'PlayGame', 'Tutorial' e 'ExitGame'.
data MenuState = MainMenu | PlayGame | Tutorial | ExitGame deriving (Eq, Show)

-- Desenha a interface do menu com base no estado atual do menu.
-- Dependendo do estado do menu, a função retorna diferentes 'Picture'.
-- Se o estado for 'MainMenu', a função desenha o menu principal com as opções de
-- iniciar o jogo, acessar o tutorial, ou sair do jogo.
-- Se o estado for 'Tutorial', a função desenha a tela do tutorial com instruções sobre o jogo.
-- Para os estados 'PlayGame' e 'ExitGame', a função retorna 'Blank', pois são placeholders.
renderMenu  :: MenuState -> Picture
renderMenu  MainMenu = Pictures
    [ Translate (-400) 150 $ Scale 0.5 0.5 $ Color green $ Text "* COMPUTING MAZE *"
    , Translate (-400) 0 $ Scale 0.5 0.5 $ Color yellow $ Text "1. Play Game"
    , Translate (-400) (-100) $ Scale 0.5 0.5 $ Color yellow $ Text "2. Tutorial"
    , Translate (-400) (-200) $ Scale 0.5 0.5 $ Color yellow $ Text "3. Exit Game"
    ]

renderMenu  PlayGame = Blank  -- Placeholder
renderMenu  Tutorial = Pictures
    [ Translate (-440) 240 $ Scale 0.5 0.5 $ Color yellow $ Text "Welcome to the Tutorial:"
    , Translate (-440) 190 $ Scale 0.2 0.2 $ Color white $ Text "Computing Maze is a 2D game for simulating mazes"
    , Translate (-440) 150 $ Scale 0.2 0.2 $ Color white $ Text "in the context of a Computer Science course. In each new level,"
    , Translate (-440) 110 $ Scale 0.2 0.2 $ Color white $ Text "you will encounter a new maze and a riddle related to a specific"
    , Translate (-440) 70 $ Scale 0.2 0.2 $ Color white $ Text "period of the course syllabus. Be cautious, your ultimate goal"
    , Translate (-440) 30 $ Scale 0.2 0.2 $ Color white $ Text "is to complete the course and successfully escape these mazes."
    , Translate (-440) (-10) $ Scale 0.2 0.2 $ Color white $ Text "The riddles you will solve are the key to permanently escaping"
    , Translate (-440) (-50) $ Scale 0.2 0.2 $ Color white $ Text "each maze. But can you uncover the mysteries of the dreaded"
    , Translate (-440) (-90) $ Scale 0.2 0.2 $ Color white $ Text "Theory of Computation? Let's find out how far you can go on"
    , Translate (-440) (-130) $ Scale 0.2 0.2 $ Color white $ Text "this journey. To move the player, you can use the standard"
    , Translate (-440) (-170) $ Scale 0.2 0.2 $ Color white $ Text "game method using the arrow keys: Up, Down, Right, Left."
    , Translate (-440) (-230) $ Scale 0.3 0.3 $ Color yellow $ Text "Press 4 to return to the main menu."
    ]
renderMenu  ExitGame = Blank  -- Placeholder

-- Lida com os eventos do menu.
-- Dependendo da tecla pressionada, a função altera o estado do menu.
-- Se a tecla '1' for pressionada, o estado muda para 'PlayGame'.
-- Se a tecla '2' for pressionada, o estado muda para 'Tutorial'.
-- Se a tecla '3' for pressionada, o estado muda para 'ExitGame'.
-- Se a tecla '4' for pressionada, o estado retorna ao 'MainMenu', mas isso só ocorre
-- se o estado atual for 'Tutorial'.
menuEventHandler :: Event -> MenuState -> MenuState
menuEventHandler (EventKey (Char '1') Down _ _) _ = PlayGame
menuEventHandler (EventKey (Char '2') Down _ _) _ = Tutorial
menuEventHandler (EventKey (Char '3') Down _ _) _ = ExitGame
menuEventHandler (EventKey (Char '4') Down _ _) _ = MainMenu
menuEventHandler _ s = s

-- Processa o estado do menu ao longo do tempo.
-- No contexto atual, essa função não altera o estado do menu, ela simplesmente retorna
-- o estado atual inalterado, pois o tempo não influencia o menu.
menuTimeHandler :: Float -> MenuState -> MenuState
menuTimeHandler _ s = s
