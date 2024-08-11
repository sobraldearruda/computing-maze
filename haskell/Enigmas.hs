module Enigmas where

import Data.Char (toLower)
import Menu (toContinue)
import System.IO (hFlush, stdout)

-- Função para mostrar um enigma genérico
mostrarEnigma :: String -> String -> String -> IO ()
mostrarEnigma titulo descricao respostaCorreta = do
    putStrLn titulo
    toContinue
    putStrLn descricao
    putStrLn "Digite sua resposta:"
    respostaUsuario <- getLine
    if map toLower respostaUsuario == map toLower respostaCorreta
        then putStrLn "Resposta correta! Você pode prosseguir."
        else putStrLn "Resposta incorreta. Tente novamente."
    toContinue

-- Enigma de Programação 1
programacao1 :: IO ()
programacao1 = mostrarEnigma
    "Parabéns, você chegou ao final do primeiro labirinto."
    "Mas você achou que seria fácil assim sair daqui? Primeiro, você precisa responder ao meu enigma de Programação 1!!!\nNeste enigma, você volta ao início do labirinto várias e várias vezes, então você é uma estrutura de dados."
    "estrutura de repetição"

-- Enigma de FMCC
fmcc :: IO ()
fmcc = mostrarEnigma
    "Você conseguiu, o segundo labirinto termina aqui."
    "Mas antes de prosseguir, responda ao enigma de FMCC! Eu sou um conceito que define uma coleção de elementos distintos. Meu símbolo geralmente é representado por uma letra maiúscula. Quem sou eu?"
    "conjunto"

-- Enigma de Introdução à Computação
introducaoAComputacao :: IO ()
introducaoAComputacao = mostrarEnigma
    "Você finalmente chegou ao último labirinto."
    "Mas para sair, você deve resolver este enigma de Introdução à Computação! Sou o primeiro passo para se comunicar com o computador. Meu nome é uma combinação de duas palavras que envolvem escrever comandos. Quem sou eu?"
    "linguagem de programação"

-- Enigma de Calculo 1
calculo1 :: IO ()
calculo1 = mostrarEnigma
    "Bem-vindo ao enigma de Cálculo 1!"
    "Qual é a derivada da função f(x) = x^2? Esta é uma questão básica de cálculo diferencial."
    "2x"

-- Enigma de  FMCC 2
fmcc2 :: IO ()
fmcc2 = mostrarEnigma
    "Você está no enigma de FMCC2!"
    "Eu sou um tipo de dado que permite armazenar valores de tipos diferentes e é imutável. Qual é o meu nome?"
    "tupla"

-- Enigma de LP 2
linguagemProgramacao2 :: IO ()
linguagemProgramacao2 = mostrarEnigma
    "Vamos testar seu conhecimento em Java adquiridos em P2!"
    "Qual é o modificador de acesso que permite que um membro de uma classe seja acessado apenas dentro da mesma classe?"
    "private"


-- Enigma de Psoft
projetoSoftware :: IO ()
projetoSoftware = mostrarEnigma
    "Você está prestes a resolver um enigma de Projeto de Software!"
    "Qual é o nome do modelo que descreve a organização do software em camadas distintas, como a camada de apresentação, a camada de lógica de negócios e a camada de dados?"
    "arquitetura em camadas"

-- Enigma de Eda
estruturasDados :: IO ()
estruturasDados = mostrarEnigma
    "Você está resolvendo um enigma de Estruturas de Dados!"
    "Qual é a estrutura de dados que usa uma abordagem LIFO (Last In, First Out) para armazenar elementos?"
    "pilha"


redesComputadores :: IO ()
redesComputadores = mostrarEnigma
    "Chegou ao enigma de Redes de Computadores!"
    "Qual é o protocolo da camada de transporte que garante a entrega ordenada e sem erros de pacotes de dados entre sistemas?"
    "TCP"