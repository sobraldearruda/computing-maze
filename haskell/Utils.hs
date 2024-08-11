module Utils where

-- Continua o fluxo de execução
toContinue :: IO ()
toContinue = do
    putStrLn "\n### Pressione ENTER para continuar. ###"
    line <- getLine :: IO String
    putStrLn "\n"

-- Exibe textos na tela
printTextScreen :: [String] -> IO ()
printTextScreen texts = do
  mapM_ putStr (drawTextScreen texts)

-- Concatena listas de strings na tela
drawTextScreen:: [String] -> [String]
drawTextScreen texts = [concat texts]