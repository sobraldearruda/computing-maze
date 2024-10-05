/* Define o módulo e os predicados exportados.*/
:- module(enigmas, [
    initial_enigma_state/2,
    render_enigma/1,
    enigma_event_handler/3,
    valid_input/2, % Nova função para validar a entrada do usuário
    enigmas/1
]).

/* Definindo o estado do enigma.
   O estado é representado como enigma_state/5.
   enigma_state(Índice do Enigma, Pergunta, Opções, Resposta Correta, Opção Selecionada).*/
:- dynamic enigma_state/5.

/* Predicado para criar o estado inicial do enigma.*/
initial_enigma_state(Index, EnigmaState) :-
    enigmas(Enigmas),
    nth0(Index, Enigmas, enigma(Question, Options, CorrectAnswer)),
    EnigmaState = enigma_state(Index, Question, Options, CorrectAnswer, -1). % -1 indica que nenhuma opção foi selecionada

/* Predicado para renderizar o enigma no terminal.*/
render_enigma(EnigmaState) :-
    EnigmaState = enigma_state(_, Question, Options, _, _),
    writeln('Enigma: '),
    writeln(Question),
    writeln('Options: '),
    display_options(Options),
    nl.

/* Predicado auxiliar para exibir as opções com índices.*/
display_options(Options) :-
    display_options(Options, 1).
display_options([], _).
display_options([Option | Rest], Index) :-
    format("~w. ~w~n", [Index, Option]),
    NextIndex is Index + 1,
    display_options(Rest, NextIndex).

/* Predicado para manipular a seleção de opções do enigma.*/
enigma_event_handler(Option, EnigmaState, UpdatedState) :-
    % Verifica se a opção é válida
    valid_input(Option, EnigmaState),
    atom_number(Option, OptionNumber),
    EnigmaState = enigma_state(Index, Question, Options, CorrectAnswer, _),
    UpdatedOption is OptionNumber - 1,  % Ajusta para índice de 0 a N
    UpdatedState = enigma_state(Index, Question, Options, CorrectAnswer, UpdatedOption).

/* Predicado para verificar se a opção selecionada é válida.*/
valid_input(Option, enigma_state(_, _, Options, _, _)) :-
    atom_number(Option, OptionNumber),  /*Tenta converter o input para número.*/
    length(Options, Length),  /*Obtém o número de opções disponíveis.*/
    OptionNumber >= 1,  /*O número deve ser pelo menos 1.*/
    OptionNumber =< Length,  /*O número deve ser no maximo o número de opções.*/
    !.
valid_input(_, _) :-  /*Se falhar, exibe mensagem de erro e falha.*/
    writeln('Invalid option! Please, write a number between 1 and 4.'), fail.

/* Lista de enigmas disponíveis no jogo.*/
enigmas([
    enigma("If you return to the start of the maze over and over again, then you are a: ",
        ["Stack", "Queue", "Loop structure", "Recursion"], 2),

    enigma("I am the following sequence of numbers: [0, 1, 1, 2, 3, 5, 8, 13, 21, ...]. Who am I? ",
        ["Fibonacci sequence", "Arithmetic sequence", "Geometric sequence", "Taylor series"], 0),

    enigma("You are the last to arrive here, but you will be the first to leave. Who are you? ",
        ["Queue", "Stack", "Linked list", "Binary tree"], 1),

    enigma("I have an end, but I follow parallel paths until I get there. Who am I? ",
        ["Turing machine", "Deterministic finite automaton", "Nondeterministic finite automaton", "Context-free grammar"], 2),

    enigma("You communicate with different platforms, acting as an intermediary between the user and the provider. Who are you? ",
        ["API", "Database", "Operating system", "Middleware"], 0),

    enigma("You don't know where to go, but you check all possible paths, possibly returning to the start of your journey. What technique are you? ",
        ["Dynamic programming", "Backtracking", "Greedy", "Breadth-first search"], 1),

    enigma("Your name is a data structure used for analysis, verification, and validation. What is your name? ",
        ["Graph", "Stack", "Syntax tree", "Priority queue"], 2),

    enigma("I follow a cache memory policy where you are the first to arrive and the first to leave. Who am I? ",
        ["LIFO", "FIFO", "MRU", "LFU"], 1),

    enigma("I am the middle, based on the beginning, upon an end. Sometimes, people call me 'reasearch'. Who am I? ",
        ["Analysis", "Methodology", "Theoretical background", "References"], 1)
]).
