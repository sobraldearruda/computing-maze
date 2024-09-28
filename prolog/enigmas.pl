% Definindo o módulo enigmas e os predicados exportados
:- module(enigmas, [
    initial_enigma_state/2,
    render_enigma/1,
    enigma_event_handler/3,
    enigmas/1
]).

% Definindo o estado do Enigma
% Estado é representado como enigma_state/6
% enigma_state(Índice do Enigma, Pergunta1, Pergunta2, Opções, Resposta Correta, Opção Selecionada)
:- dynamic enigma_state/6.

% Função para criar o estado inicial do enigma
initial_enigma_state(Index, EnigmaState) :-
    enigmas(Enigmas),
    nth0(Index, Enigmas, Enigma),
    Enigma = enigma(Question1, Question2, Options, CorrectAnswer),
    EnigmaState = enigma_state(Index, Question1, Question2, Options, CorrectAnswer, -1).

% Função para renderizar o enigma no terminal
render_enigma(EnigmaState) :-
    EnigmaState = enigma_state(_, Question1, Question2, Options, _, SelectedOption),
    writeln('Pergunta 1:'), writeln(Question1),
    writeln('Pergunta 2:'), writeln(Question2),
    writeln('Opções de resposta:'),
    display_options(Options, 1),
    writeln('Selecionado: '), write(SelectedOption),
    nl.

display_options([], _).
display_options([Option | Rest], Index) :-
    format("~w. ~w~n", [Index, Option]),
    NextIndex is Index + 1,
    display_options(Rest, NextIndex).

% Função para manipular a seleção de opções do enigma
enigma_event_handler('1', EnigmaState, UpdatedState) :-
    EnigmaState = enigma_state(Index, Question1, Question2, Options, CorrectAnswer, _),
    UpdatedState = enigma_state(Index, Question1, Question2, Options, CorrectAnswer, 0).

enigma_event_handler('2', EnigmaState, UpdatedState) :-
    EnigmaState = enigma_state(Index, Question1, Question2, Options, CorrectAnswer, _),
    UpdatedState = enigma_state(Index, Question1, Question2, Options, CorrectAnswer, 1).

enigma_event_handler('3', EnigmaState, UpdatedState) :-
    EnigmaState = enigma_state(Index, Question1, Question2, Options, CorrectAnswer, _),
    UpdatedState = enigma_state(Index, Question1, Question2, Options, CorrectAnswer, 2).

enigma_event_handler('4', EnigmaState, UpdatedState) :-
    EnigmaState = enigma_state(Index, Question1, Question2, Options, CorrectAnswer, _),
    UpdatedState = enigma_state(Index, Question1, Question2, Options, CorrectAnswer, 3).

enigma_event_handler(_, EnigmaState, EnigmaState).

% Lista de enigmas disponível no jogo
enigmas([
    enigma("Se você volta ao início do labirinto repetidamente,", "então você é um(a):",
        ["Pilha", "Fila", "Estrutura de laço", "Recursão"], 2),

    enigma("Sou a seguinte sequência de números: [0, 1, 1, 2, 3, 5, 8, 13, 21, ...].", "Quem sou eu?",
        ["Sequência de Fibonacci", "Sequência aritmética", "Sequência geométrica", "Série de Taylor"], 0),

    enigma("Você é o último a chegar aqui, mas será o primeiro a sair.", "Quem é você?",
        ["Fila", "Pilha", "Lista encadeada", "Árvore binária"], 1),

    enigma("Tenho um fim, mas sigo caminhos paralelos até chegar lá.", "Quem sou eu?",
        ["Máquina de Turing", "Autômato finito determinístico", "Autômato finito não determinístico", "Gramática livre de contexto"], 2),

    enigma("Você se comunica com diferentes plataformas, agindo como um intermediário entre o usuário e o provedor. Quem é você?",
        ["API", "Banco de dados", "Sistema operacional", "Middleware"], 0),

    enigma("Você não sabe para onde ir, mas verifica todos os caminhos possíveis, possivelmente voltando ao início da sua jornada. Que técnica é você?",
        ["Programação dinâmica", "Backtracking", "Guloso", "Busca em largura"], 1),

    enigma("Seu nome é uma estrutura de dados usada para análise, verificação e validação. Qual é o seu nome?",
        ["Grafo", "Pilha", "Árvore sintática", "Fila de prioridade"], 2),

    enigma("Eu sigo uma política de memória cache onde você é o primeiro a chegar e o primeiro a sair. Quem sou eu?",
        ["LIFO", "FIFO", "MRU", "LFU"], 1),

    enigma("Eu sou o meio, com base no início, ao final de um caminho. Às vezes, me chamam de 'reasearch'. Quem sou eu?",
        ["Análise", "Metodologia", "Referencial teórico", "Referências"], 1)
]).
