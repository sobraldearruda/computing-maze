module AppRendering (AppState (..), drawAppState, appEventHandler, appTimeHandler) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Rendering (GlossState(..), initialGlossState, drawGlossState, glossEventHandler, glossTimeHandler)
import Menu (MenuState (..), renderMenu, menuEventHandler)
import Debug.Trace (trace)

-- Estado do jogo
data AppState = AppState
  { menuState :: MenuState
  , gameState :: Maybe GlossState
  } deriving (Eq, Show)

-- Função que desenha o estado atual do aplicativo
drawAppState :: AppState -> Picture
drawAppState state = trace ("Drawing state: " ++ show state) $
  case state of
    AppState MainMenu _ -> renderMenu MainMenu
    AppState PlayGame Nothing -> Blank
    AppState PlayGame (Just gs) -> drawGlossState gs
    AppState Tutorial _ -> renderMenu Tutorial
    AppState ExitGame _ -> Blank

-- Função que lida com os eventos do aplicativo
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

-- Função que lida com o tempo do aplicativo
appTimeHandler :: Float -> AppState -> AppState
appTimeHandler dt (AppState PlayGame (Just gs)) =
  AppState PlayGame (Just (glossTimeHandler dt gs))

appTimeHandler _ s = s
