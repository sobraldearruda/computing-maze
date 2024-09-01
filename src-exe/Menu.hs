module Menu where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data MenuState = MainMenu | PlayGame | Tutorial | ExitGame deriving (Eq, Show)

-- Desenha o menu principal
renderMenu  :: MenuState -> Picture
renderMenu  MainMenu = Pictures
    [ Translate (-150) 200 $ Scale 0.5 0.5 $ Color white $ Text "1. PlayGame"
    , Translate (-150) 100  $ Scale 0.5 0.5 $ Color white $ Text "2. Tutorial"
    , Translate (-150) 0   $ Scale 0.5 0.5 $ Color white $ Text "3. ExitGame"
    ]

renderMenu  PlayGame = Blank  -- Placeholder
renderMenu  Tutorial = Pictures
    [ Translate (-900) 310 $ Scale 0.5 0.5 $ Color white $ Text "Welcome to the Tutorial!"
    , Translate (-900) 270 $ Scale 0.2 0.2 $ Color white $ Text "Computing Maze é um jogo 2D de simulação de labirintos em um contexto do curso de Ciência da Computação."
    , Translate (-900) 240 $ Scale 0.2 0.2 $ Color white $ Text "Os labirintos sao gerados automaticamente de forma aleatória."
    , Translate (-900) 210 $ Scale 0.2 0.2 $ Color white $ Text "A cada fase nova, você vai encontrar um novo labirinto e um enigma relacionado a um período específico do fluxograma do curso."
    , Translate (-900) 180 $ Scale 0.2 0.2 $ Color white $ Text "Seja cauteloso, o seu objetivo final é chegar a conclusão do curso e conseguir sair desses labirintos com sucesso."
    , Translate (-900) 150 $ Scale 0.2 0.2 $ Color white $ Text "Os enigmas que você vai desvendar são a chave para conseguir sair definitivamente de cada labirinto."
    , Translate (-900) 120 $ Scale 0.2 0.2 $ Color white $ Text "Mas será que você consegue descobrir os mistérios de FMCC?"
    , Translate (-900) 90 $ Scale 0.2 0.2 $ Color white $ Text "Ou será que você vai conseguir solucionar a temida Teoria da Computação?"
    , Translate (-900) 60 $ Scale 0.2 0.2 $ Color white $ Text "Você acha mesmo ser capaz de desconstruir os quebra-cabeças de ATAL e Compiladores?"
    , Translate (-900) 30 $ Scale 0.2 0.2 $ Color white $ Text "Vamos descobrir até onde você consegue chegar nessa jornada."
    , Translate (-900) 0 $ Scale 0.2 0.2 $ Color white $ Text "Para movimentar o player, pode ser usado o método padrão de jogo utilizando as teclas (A, W, D, S) ou (setas ⬅, ⬆, ⮕, ⬇)."
    , Translate (-900) (-100) $ Scale 0.4 0.4 $ Color white $ Text "Press 4 to return to the main menu."
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