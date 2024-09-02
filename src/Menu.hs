module Menu (
    MenuState (..),
    renderMenu,
    menuEventHandler,
    menuTimeHandler
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data MenuState = MainMenu | PlayGame | Tutorial | ExitGame deriving (Eq, Show)

-- Desenha o menu principal
renderMenu  :: MenuState -> Picture
renderMenu  MainMenu = Pictures
    [ Translate (-400) 150 $ Scale 0.5 0.5 $ Color green $ Text "* COMPUTING MAZE *"
    , Translate (-400) 0  $ Scale 0.5 0.5 $ Color yellow $ Text "1. Play Game"
    , Translate (-400) (-100)   $ Scale 0.5 0.5 $ Color yellow $ Text "2. Tutorial"
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

-- Manipula os eventos do menu
menuEventHandler :: Event -> MenuState -> MenuState
menuEventHandler (EventKey (Char '1') Down _ _) _ = PlayGame
menuEventHandler (EventKey (Char '2') Down _ _) _ = Tutorial
menuEventHandler (EventKey (Char '3') Down _ _) _ = ExitGame
menuEventHandler (EventKey (Char '4') Down _ _) _ = MainMenu -- Só é ativado quando está na aba de Tutorial
menuEventHandler _ s = s

-- Função para processar o estado do menu (neste caso, não faz nada com o tempo)
menuTimeHandler :: Float -> MenuState -> MenuState
menuTimeHandler _ s = s
