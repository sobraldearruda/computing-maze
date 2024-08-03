module Enigmas where

import Menu (toContinue)

programacao1 :: IO ()
programacao1 = do
    putStrLn "Parabéns, você chegou ao final do primeiro labirinto."
    putStrLn "Mas você achou que seria fácil assim sair daqui?"
    putStrLn "Primeiro, você precisa responder ao meu enigma de Programação 1!!!"
    toContinue
    putStrLn "Neste enigma, você volta ao início do labirinto"
    putStrLn "várias e várias vezes, então você é uma estrutura de dados."
    -- RESPOSTA: ESTRUTURA DE REPETIÇÃO
    toContinue

fmcc :: IO ()
fmcc = do
    putStrLn "Você conseguiu, o segundo labirinto termina aqui."
    -- CONTINUAR
    toContinue
