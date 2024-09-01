module Enigmas (EnigmaState(..), initialEnigmaState, renderEnigma, enigmaEventHandler, enigmas) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data EnigmaState = EnigmaState
  { enigmaIndex :: Int
  , enigmaQuestion :: String
  , enigmaOptions :: [String]
  , correctAnswer :: Int
  , selectedOption :: Int
  } deriving (Eq, Show)

-- Função para criar um estado inicial do Enigma usando o enigma do índice 0
initialEnigmaState :: Int -> EnigmaState
initialEnigmaState n = 
  let index = n 
  in EnigmaState  
  { enigmaIndex = index
  , enigmaQuestion = question (enigmas !! index)
  , enigmaOptions = options (enigmas !! index)
  , correctAnswer = correctAnswerIndex (enigmas !! index)
  , selectedOption = -1  -- Nenhuma opção selecionada
  }

-- Função para renderizar o enigma na tela
renderEnigma :: EnigmaState -> Picture
renderEnigma enigmaState = Pictures
  [ Translate (-500) 200 $ Scale 0.15 0.15 $ Color white $ Text (enigmaQuestion enigmaState)
  , Translate (-500) 150 $ Scale 0.15 0.15 $ Color white $ Text ("1. " ++ (enigmaOptions enigmaState !! 0))
  , Translate (-500) 100 $ Scale 0.15 0.15 $ Color white $ Text ("2. " ++ (enigmaOptions enigmaState !! 1))
  , Translate (-500) 50  $ Scale 0.15 0.15 $ Color white $ Text ("3. " ++ (enigmaOptions enigmaState !! 2))
  , Translate (-500) 0   $ Scale 0.15 0.15 $ Color white $ Text ("4. " ++ (enigmaOptions enigmaState !! 3))
  , Translate (-500) (-100) $ Scale 0.15 0.15 $ Color yellow $ Text ("Selected: " ++ show (selectedOption enigmaState + 1))
  , Translate (-500) (-150) $ Scale 0.15 0.15 $ Color yellow $ Text ("Pressione 'Enter' para confirmar sua resposta")
  ]

-- Função para manipular a seleção do enigma
enigmaEventHandler :: Event -> EnigmaState -> EnigmaState
enigmaEventHandler (EventKey (Char '1') Down _ _) enigmaState = enigmaState { selectedOption = 0 }
enigmaEventHandler (EventKey (Char '2') Down _ _) enigmaState = enigmaState { selectedOption = 1 }
enigmaEventHandler (EventKey (Char '3') Down _ _) enigmaState = enigmaState { selectedOption = 2 }
enigmaEventHandler (EventKey (Char '4') Down _ _) enigmaState = enigmaState { selectedOption = 3 }
enigmaEventHandler _ enigmaState = enigmaState

data Enigma = Enigma
  { question :: String            -- A pergunta do enigma
  , options  :: [String]          -- As opções de resposta
  , correctAnswerIndex :: Int     -- O índice da resposta correta
  }

type Enigmas = [Enigma]

enigmas :: Enigmas
enigmas =
  [ Enigma "Se você volta ao início do labirinto várias e várias vezes, então você é uma:"
      ["Pilha", "Fila", "Estrutura de repetição", "Recursão"]
      2

  , Enigma "Eu sou a seguinte sequência de números: [0, 1, 1, 2, 3, 5, 8, 13, 21, ...]. Quem sou eu?"
      ["Sequência de Fibonacci", "Sequência aritmética", "Sequência geométrica", "Série de Taylor"]
      0

  , Enigma "Você é o último a chegar aqui, mas será o primeiro a sair. Quem sou eu?"
      ["Fila", "Pilha", "Lista encadeada", "Árvore binária"]
      1

  , Enigma "Eu tenho um fim, mas eu sigo caminhos paralelos até chegar lá. Quem sou eu?"
      ["Máquina de Turing", "Autômato finito determinístico", "Autômato finito não determinístico", "Gramática livre de contexto"]
      2

  , Enigma "Você se comunica com plataformas diferentes, servindo como um intermediário entre quem usa e quem fornece. Quem sou eu?"
      ["API", "Banco de dados", "Sistema operacional", "Middleware"]
      0

  , Enigma "Se você não sabe aonde ir, mas pode verificar todos os caminhos possíveis, na pior das hipóteses, precisará voltar ao início de sua jornada. Qual técnica está sendo descrita?"
      ["Programação dinâmica", "Backtracking", "Greedy", "Busca em largura"]
      1

  , Enigma "Seu nome é uma estrutura de dados usada para análise, verificação e validação. Quem sou eu?"
      ["Grafo", "Pilha", "Árvore sintática", "Fila de prioridade"]
      2

  , Enigma "Eu sigo uma política de cache de memória na qual você é o primeiro a chegar e o primeiro a sair. Quem sou eu?"
      ["LIFO", "FIFO", "MRU", "LFU"]
      1
  ]