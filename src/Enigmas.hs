module Enigmas (
  EnigmaState(..), 
  initialEnigmaState, 
  renderEnigma, 
  enigmaEventHandler, 
  enigmas
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data EnigmaState = EnigmaState
  { enigmaIndex :: Int
  , enigmaQuestion1  :: String
  , enigmaQuestion2  :: String
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
  , enigmaQuestion1 = question1 (enigmas !! index)
  , enigmaQuestion2 = question2 (enigmas !! index)
  , enigmaOptions = options (enigmas !! index)
  , correctAnswer = correctAnswerIndex (enigmas !! index)
  , selectedOption = -1  -- Nenhuma opção selecionada
  }

-- Função para renderizar o enigma na tela
renderEnigma :: EnigmaState -> Picture
renderEnigma enigmaState = Pictures
  [ Translate (-430) 200 $ Scale 0.15 0.15 $ Color white $ Text (enigmaQuestion1 enigmaState)
  , Translate (-430) 170 $ Scale 0.15 0.15 $ Color white $ Text (enigmaQuestion2 enigmaState)
  , Translate (-430) 100 $ Scale 0.15 0.15 $ Color white $ Text ("1. " ++ (enigmaOptions enigmaState !! 0))
  , Translate (-430) 50 $ Scale 0.15 0.15 $ Color white $ Text ("2. " ++ (enigmaOptions enigmaState !! 1))
  , Translate (-430) 0  $ Scale 0.15 0.15 $ Color white $ Text ("3. " ++ (enigmaOptions enigmaState !! 2))
  , Translate (-430) (-50)   $ Scale 0.15 0.15 $ Color white $ Text ("4. " ++ (enigmaOptions enigmaState !! 3))
  , Translate (-430) (-150) $ Scale 0.15 0.15 $ Color yellow $ Text ("Selected: " ++ show (selectedOption enigmaState + 1))
  , Translate (-430) (-200) $ Scale 0.15 0.15 $ Color yellow $ Text "Press 'Enter' to confirm your answer."
  ]

-- Função para manipular a seleção do enigma
enigmaEventHandler :: Event -> EnigmaState -> EnigmaState
enigmaEventHandler (EventKey (Char '1') Down _ _) enigmaState = enigmaState { selectedOption = 0 }
enigmaEventHandler (EventKey (Char '2') Down _ _) enigmaState = enigmaState { selectedOption = 1 }
enigmaEventHandler (EventKey (Char '3') Down _ _) enigmaState = enigmaState { selectedOption = 2 }
enigmaEventHandler (EventKey (Char '4') Down _ _) enigmaState = enigmaState { selectedOption = 3 }
enigmaEventHandler _ enigmaState = enigmaState

data Enigma = Enigma
  { question1 :: String            -- A pergunta do enigma
  , question2 :: String            -- Continuação da pergunta do enigma
  , options  :: [String]           -- As opções de resposta
  , correctAnswerIndex :: Int      -- O índice da resposta correta
  }

type Enigmas = [Enigma]

enigmas :: Enigmas
enigmas =
  [ Enigma "If you return to the start of the maze over and over again,"
      "then you are a:"
      ["Stack", "Queue", "Loop structure", "Recursion"]
      2

  , Enigma "I am the following sequence of numbers: [0, 1, 1, 2, 3, 5, 8, 13, 21, ...]."
      "Who am I?"
      ["Fibonacci sequence", "Arithmetic sequence", "Geometric sequence", "Taylor series"]
      0

  , Enigma "You are the last to arrive here, but you will be the first to leave."
      "Who are you?"
      ["Queue", "Stack", "Linked list", "Binary tree"]
      1

  , Enigma "I have an end, but I follow parallel paths until I get there."
      "Who am I?"
      ["Turing machine", "Deterministic finite automaton", "Nondeterministic finite automaton", "Context-free grammar"]
      2

  , Enigma "You communicate with different platforms, acting as an"
      "intermediary between the user and the provider. Who are you?"
      ["API", "Database", "Operating system", "Middleware"]
      0

  , Enigma "You don't know where to go, but you check all possible paths,"
      "possibly returning to the start of your journey. What technique are you?"
      ["Dynamic programming", "Backtracking", "Greedy", "Breadth-first search"]
      1

  , Enigma "Your name is a data structure used for analysis, verification,"
      "and validation. What is your name?"
      ["Graph", "Stack", "Syntax tree", "Priority queue"]
      2

  , Enigma "I follow a cache memory policy where you are the first to arrive"
      "and the first to leave. Who am I?"
      ["LIFO", "FIFO", "MRU", "LFU"]
      1
  
  , Enigma "I am the middle, based on the beginning, upon an end."
      "Sometimes, people call me 'reasearch'. Who am I?"
      ["Analysis", "Methodology", "Theoretical background", "References"]
      1
  , Enigma ""
      ""
      []
      3
  ]
