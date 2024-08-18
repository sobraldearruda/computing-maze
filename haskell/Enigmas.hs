module Enigmas where

import Utils (toContinue, printTextScreen, drawTextScreen)
import Data.Char (toLower)

-- Exibe os Enigmas
printEnigma :: String -> [String] -> String -> IO ()
printEnigma titulo descricao respostaCorreta = do
    putStrLn titulo
    toContinue
    putStrLn (concat descricao)
    toContinue
    putStrLn "Digite sua resposta: "
    respostaUsuario <- getLine
    if map toLower respostaUsuario == map toLower respostaCorreta
        then putStrLn "Você mandou muito bem! Agora sim, você pode continuar."
        else putStrLn "Deu ruim pra você! Acho que precisa estudar um pouco mais. Tente novamente."
    toContinue

-- Enigma de "Programação 1"
programacao1 :: IO ()
programacao1 = printEnigma
    "Eu sou o Enigma de Programação 1"
    ["Parabéns, você chegou ao final do primeiro labirinto. ",
    "Mas você achou que seria fácil assim sair daqui? ",
    "Primeiro, você precisa responder ao meu enigma: ",
    "Se você volta ao início do labirinto várias e várias vezes, ",
    "então você é uma estrutura de dados."]
    "estrutura de repetição"

-- Enigma de "Fundamentos de Matemática para Ciência da Computação 2"
fmcc2 :: IO ()
fmcc2 = printEnigma
    "Eu sou o Enigma de FMCC 2"
    ["Você conseguiu, o segundo labirinto termina aqui. ",
    "Mas antes de prosseguir, será que você é louco o suficiente ",
    "para resolver o meu enigma? ",
    "Eu sou a seguinte sequência de números: [0, 1, 1, 2, 3, 5, 8, 13, 21, ...]."]
    "sequência de fibonacci"

-- Enigma de "Estruturas de Dados e Algoritmos"
estruturasDados :: IO ()
estruturasDados = printEnigma
    "Eu sou o Enigma de Estruturas de Dados e Algoritmos"
    ["Você achou mesmo que conseguiria passar pelo labirinto sem antes passar por mim? ",
    "Vamos ver se você realmente é inteligente o quanto pensa: ",
    "Você é o último a chegar aqui, mas será o primeiro a sair."]
    "pilha"

-- Enigma de "Teoria da Computação"
teoriaComputacao :: IO ()
teoriaComputacao = printEnigma
    "Eu sou o Enigma de Teoria da Computação"
    ["Agora chegou a sua vez de desvendar o meu enigma, ",
    "aquele que todos têm medo de enfrentar, mas que é essencial em sua jornada: ",
    "Eu tenho um fim, mas eu sigo caminhos paralelos até chegar lá."]
    "autômato finito não determinístico"

-- Enigma de "Projeto de Software"
projetoSoftware :: IO ()
projetoSoftware = printEnigma
    "Eu sou o enigma de Projeto de Software"
    ["Vejo que você é perspicaz o suficiente para achar que pode passar por mim. ",
    "Será que você consegue resolver o meu enigma? ",
    "Você se comunica com plataformas diferentes, servindo como um intermediário entre ",
    "quem usa e quem fornece. Você gosta de compartilhar dados e recursos, ",
    "mas depende de muitas interações."]
    "api" -- API: Application Programming Interface

-- Enigma de "Análise e Técnicas de Algoritmos"
analiseAlgoritmos :: IO ()
analiseAlgoritmos = printEnigma
    "Eu sou o enigma de Análise e Técnicas de Algoritmos"
    ["Parece que você está indo muito bem, mas duvido que você consiga passar por mim. ",
    "O meu enigma vai te fazer pensar se realmente vale a pena seguir em frente: ",
    "Se você não sabe aonde ir, mas pode verificar todos os caminhos possíveis, ",
    "na pior das hipóteses, então, vai precisar voltar ao início de sua jornada, ",
    "ou até verificar com antecedência qual caminho é válido."]
    "backtracking"

-- Enigma de "Compiladores"
compiladores :: IO ()
compiladores = printEnigma
    "Eu sou o enigma de Compiladores"
    ["Se você chegou até aqui, então é um ótimo jogador. ",
    "Mas acho muito difícil você conseguir solucionar o meu enigma: ",
    "Seu nome é uma estrutura de dados usada para análise, verificação e validação, ",
    "então se você é uma lista de tuplas que formam uma matriz de coordenadas, ",
    "sua representação pode ser um labirinto ou o seu próprio nome."]
    "árvore sintática"

-- Enigma de "Projeto de Computação"
projetoComputacao :: IO ()
projetoComputacao = printEnigma
    "Eu sou o enigma de Projeto de Computação"
    ["Eu duvidei que você conseguiria chegar até aqui, mas tenho certeza que ",
    "não vai conseguir resolver o meu enigma: ",
    "Eu sou um design de Projeto de Software e sigo uma política de cache de memória ",
    "na qual você é o primeiro a chegar e o primeiro a sair."]
    "fifo" -- FIFO: First-in, First-out

-- Enigma de "Trabalho de Conclusão de Curso"
tcc :: IO ()
tcc = printEnigma
    "Eu sou o enigma de Trabalho de Conclusão de Curso"
    ["Enfim, você conseguiu chegar ao último enigma dessa jornada. ",
    "Para muitos, eu sou o mais temido, mas confesso que tenho tudo a ver ",
    "com todos os outros enigmas que você já solucionou. Então, essa é a sua ",
    "chance de provar pela última vez que merece sair do labirinto e chegar a ",
    "tão sonhada conclusão do curso de Ciência da Computação. ",
    "Uma dica: a resposta correta para o meu enigma é essencial em todo tipo de pesquisa: ",
    "Eu sou o meio, diante do início, com vistas a um fim."]
    "metodologia"
