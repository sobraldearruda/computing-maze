module Menu (printMenu, chooseOption, backToMenu, startGame) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Game (displayWindow, initialize, render, handleKeys, beginTimer, sMove, pacScore, pacEat, pacEatP, wallCollision, checkTeleport, execM)
import BossAI (releasePinky, bossModeSwitch, bossCollisionSwitch, moveToTargetB, moveToTargetP)
import Utils (toContinue, printTextScreen, fps, MazeGame(..))
import Enigmas (printEnigma)
import System.Exit (exitSuccess)

-- Exibe o Menu de opções
printMenu :: IO ()
printMenu = do
    printTextScreen 
        ["###### COMPUTING MAZE ######",
        "1 - INICIAR O JOGO", 
        "2 - TUTORIAL", 
        "3 - SAIR DO JOGO"]
    option <- readLn :: IO Int
    chooseOption option

-- Seleciona a opção do usuário
chooseOption :: Int -> IO ()
chooseOption 1 = startGame
chooseOption 2 = openTutorial
chooseOption 3 = do
    putStrLn "\n###### COMPUTING MAZE ######\n"
    exitSuccess
chooseOption invalidOption = do
  putStrLn "\n### Opção inválida. Tente novamente. ###"
  printMenu

-- Retorna ao Menu de opções
backToMenu :: IO ()
backToMenu = do
    putStrLn "\n### Pressione ENTER para voltar ao MENU PRINCIPAL. ###"
    line <- getLine :: IO String
    printMenu

-- Inicia o Jogo
startGame :: IO ()
startGame = do
    putStrLn "\n### Iniciando o Jogo... ###" -- EDITAR MENSAGEM POSTERIORMENTE
    play displayWindow black fps initialize render handleKeys step
        where
            step :: Float -> MazeGame -> MazeGame
            step sec game = sMove sec $ releasePinky $ moveToTargetP $ moveToTargetB $ bossModeSwitch $ beginTimer $ bossCollisionSwitch $ pacScore $ pacEat $ pacEatP $ wallCollision $ execM $ checkTeleport game

-- Abre o Tutorial
openTutorial :: IO ()
openTutorial = do
    -- EDITAR MENSAGENS POSTERIORMENTE
    printTextScreen 
        ["Computing Maze é um jogo 2D de simulação de labirintos ",
        "em um contexto do curso de Ciência da Computação. ",
        "Os labirintos são gerados automaticamente de forma aleatória. ",
        "A cada fase nova, você vai encontrar um novo labirinto e ",
        "um enigma relacionado a um período específico do fluxograma do curso. ",
        "Seja cauteloso, o seu objetivo final é chegar a conclusão do curso ",
        "e conseguir sair desses labirintos com sucesso."]
    toContinue
    -- EDITAR MENSAGENS POSTERIORMENTE
    printTextScreen 
        ["Os enigmas que você vai desvendar são a chave ",
        "para conseguir sair definitivamente de cada labirinto. ",
        "Mas será que você consegue descobrir os mistérios de FMCC? ",
        "Ou será que você vai conseguir solucionar a temida ",
        "Teoria da Computação? Você acha mesmo ser capaz de ",
        "desconstruir os quebra-cabeças de ATAL e Compiladores? ",
        "Vamos descobrir até onde você consegue chegar nessa jornada."]
    toContinue
    -- EDITAR MENSAGEM POSTERIORMENTE
    printTextScreen 
        ["ESCREVER INSTRUÇÕES AQUI."]
    backToMenu