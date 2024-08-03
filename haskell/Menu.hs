module Menu (printMenu, chooseOption, toContinue, backToMenu) where

import System.Exit (exitSuccess)

-- EXIBE O MENU
printMenu :: IO ()
printMenu = do
    putStrLn "\n###### COMPUTING MAZE ######"
    putStrLn "1 - INICIAR O JOGO"
    putStrLn "2 - TUTORIAL"
    putStrLn "3 - SAIR DO JOGO"
    option <- readLn :: IO Int
    chooseOption option

-- SELECIONA A OPÇÃO DO USUÁRIO
chooseOption :: Int -> IO ()
chooseOption 1 = startGame
chooseOption 2 = openTutorial
chooseOption 3 = do
    putStrLn "\n###### COMPUTING MAZE ######\n"
    exitSuccess
chooseOption invalidOption = do
  putStrLn "\n### Opção inválida. Tente novamente. ###"
  printMenu

-- CONTINUA O FLUXO DE EXECUÇÃO
toContinue :: IO ()
toContinue = do
    putStrLn "\n### Pressione ENTER para continuar. ###"
    line <- getLine :: IO String
    putStrLn "\n"

-- RETORNA AO MENU DE OPÇÕES
backToMenu :: IO ()
backToMenu = do
    putStrLn "\n### Pressione ENTER para voltar ao MENU PRINCIPAL. ###"
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
    -- EDITAR MENSAGENS POSTERIORMENTE
    putStrLn "\nComputing Maze é um jogo 2D de simulação de labirintos "
    putStrLn "em um contexto do curso de Ciência da Computação."
    putStrLn "Os labirintos são gerados automaticamente de forma aleatória."
    putStrLn "A cada fase nova, você vai encontrar um novo labirinto e"
    putStrLn "um enigma relacionado a um período específico do fluxograma do curso."
    putStrLn "Seja cauteloso, o seu objetivo final é chegar a conclusão do curso"
    putStrLn "e conseguir sair desses labirintos com sucesso."
    toContinue
    -- EDITAR MENSAGENS POSTERIORMENTE
    putStrLn "\nOs enigmas que você vai desvendar são a chave"
    putStrLn "para conseguir sair definitivamente de cada labirinto."
    putStrLn "Mas será que você consegue descobrir os mistérios de FMCC?"
    putStrLn "Ou será que você vai conseguir solucionar a temida"
    putStrLn "Teoria da Computação? Você acha mesmo ser capaz de"
    putStrLn "solucionar os quebra-cabeças de ATAL e Compiladores?"
    putStrLn "Vamos descobrir até onde você consegue chegar nessa jornada."
    toContinue
    -- EDITAR MENSAGEM POSTERIORMENTE
    putStrLn "\n ESCREVER INSTRUÇÕES AQUI. "
    
    backToMenu
