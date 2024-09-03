module AppRendering (
  AppState (..), 
  drawAppState, 
  appEventHandler, 
  appTimeHandler
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Rendering (GlossState(..), initialGlossState, drawGlossState, glossEventHandler, glossTimeHandler)
import Menu (MenuState (..), renderMenu, menuEventHandler)
import Debug.Trace (trace)

-- Estado do jogo.
-- Define o estado do aplicativo com dois campos:
-- menuState: Representa o estado atual do menu.
-- gameState: Representa o estado atual do jogo (pode ser Nothing se o jogo ainda não tiver iniciado).
data AppState = AppState
  { menuState :: MenuState
  , gameState :: Maybe GlossState
  } deriving (Eq, Show)

-- Função que desenha o estado atual do aplicativo.
-- Dependendo do estado do aplicativo, esta função desenha o menu ou o estado do jogo.
-- Se o estado for MainMenu, desenha o menu principal.
-- Se o estado for PlayGame e o jogo não tiver sido inicializado, desenha uma tela em branco.
-- Se o estado for PlayGame e o jogo estiver em andamento, desenha o estado atual do jogo.
-- Se o estado for Tutorial, desenha o menu do tutorial.
-- Se o estado for ExitGame, desenha uma tela em branco.
drawAppState :: AppState -> Picture
drawAppState state = trace ("Drawing state: " ++ show state) $
  case state of
    AppState MainMenu _ -> renderMenu MainMenu
    AppState PlayGame Nothing -> Blank
    AppState PlayGame (Just gs) -> drawGlossState gs
    AppState Tutorial _ -> renderMenu Tutorial
    AppState ExitGame _ -> Blank

-- Função que lida com os eventos do aplicativo.
-- Dependendo do estado atual e do evento ocorrido, esta função atualiza o estado do aplicativo.
-- No estado MainMenu, pode iniciar o jogo, sair do jogo ou abrir o tutorial.
-- No estado PlayGame, delega o tratamento do evento para a função glossEventHandler.
-- No estado Tutorial, pode retornar ao menu principal.
appEventHandler :: Event -> AppState -> AppState
appEventHandler event (AppState MainMenu Nothing) =
  let newMenuState = menuEventHandler event MainMenu
  in case newMenuState of
       PlayGame -> AppState PlayGame (Just (initialGlossState 0))
       ExitGame -> AppState ExitGame Nothing
       Tutorial -> AppState Tutorial Nothing
       MainMenu -> AppState MainMenu Nothing

appEventHandler event (AppState PlayGame (Just gs)) =
  AppState PlayGame (Just (glossEventHandler event gs))

appEventHandler event (AppState Tutorial _) =
  let newMenuState = menuEventHandler event Tutorial
  in case newMenuState of
       MainMenu -> AppState MainMenu Nothing
       _        -> AppState Tutorial Nothing

appEventHandler _ s = s

-- Função que lida com o tempo do aplicativo.
-- Atualiza o estado do jogo com base na passagem do tempo.
-- Somente o estado PlayGame é afetado pela passagem do tempo, onde a função glossTimeHandler 
-- é usada para atualizar o estado do jogo.
appTimeHandler :: Float -> AppState -> AppState
appTimeHandler dt (AppState PlayGame (Just gs)) =
  AppState PlayGame (Just (glossTimeHandler dt gs))

appTimeHandler _ s = s
