module Menu where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data MenuState = MainMenu | PlayGame | Tutorial | ExitGame deriving (Eq, Show)

-- Desenha o menu principal
renderMenu  :: MenuState -> Picture
renderMenu  MainMenu = Pictures
    [ Translate (-150) 200 $ Scale 0.5 0.5 $ Color white $ Text "1. Play Game"
    , Translate (-150) 100  $ Scale 0.5 0.5 $ Color white $ Text "2. Tutorial"
    , Translate (-150) 0   $ Scale 0.5 0.5 $ Color white $ Text "3. Exit Game"
    ]
renderMenu  PlayGame = Blank  -- Placeholder
renderMenu  Tutorial = Blank  -- Placeholder para o tutorial
renderMenu  ExitGame = Blank  -- Placeholder

-- Manipula os eventos do menu
menuEventHandler :: Event -> MenuState -> MenuState
menuEventHandler (EventKey (Char '1') Down _ _) _ = PlayGame
menuEventHandler (EventKey (Char '2') Down _ _) _ = Tutorial
menuEventHandler (EventKey (Char '3') Down _ _) _ = ExitGame
menuEventHandler _ s = s

-- Função para processar o estado do menu (neste caso, não faz nada com o tempo)
menuTimeHandler :: Float -> MenuState -> MenuState
menuTimeHandler _ s = s