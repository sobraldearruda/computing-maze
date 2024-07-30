module Menu (printMenu, chooseOption) where

import System.Exit

-- EXIBE O MENU
printMenu :: IO ()
printMenu = do
    putStrLn "###### COMPUTING MAZE ######"
    putStrLn "1 - INICIAR O JOGO"
    putStrLn "2 - TUTORIAL"
    putStrLn "3 - SAIR"
    option <- readLn :: IO Int
    chooseOption option

-- SELECIONA A OPÇÃO DO USUÁRIO
chooseOption :: Int -> IO ()
chooseOption 1 = startGame
chooseOption 2 = openTutorial
chooseOption 3 = exitSuccess
chooseOption invalidOption = do
  putStrLn "### Opção inválida. Tente novamente. ###"
  printMenu

-- CONTINUA O FLUXO DE EXECUÇÃO
toContinue :: IO ()
toContinue = do
    putStrLn "\n### Pressione ENTER para continuar. ###"
    line <- getLine :: IO String
    putStrLn "\n\n"

-- RETORNA AO MENU DE OPÇÕES
backToMenu :: IO ()
backToMenu = do
    putStrLn "\n### Pressione ENTER para voltar ao MENU PRINCIPAL. ==="
    line <- getLine :: IO String
    printMenu

-- INICIA O JOGO
startGame :: IO ()
startGame = do
    putStrLn "\n### Escrever algo aqui. ###" -- EDITAR MENSAGEM POSTERIORMENTE
    toContinue

-- ABRE O TUTORIAL
openTutorial :: IO ()
openTutorial = do
    putStrLn "### Escrever algo aqui. ###" -- EDITAR MENSAGEM POSTERIORMENTE
    toContinue
    putStrLn "### Escrever algo aqui. ###" -- EDITAR MENSAGEM POSTERIORMENTE

    -- REPETIR LINHAS ACIMA DE ACORDO COM AS INSFORMAÇÕES DO JOGO
    
    backToMenu
