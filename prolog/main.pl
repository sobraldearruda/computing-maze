% main.pl
:- consult('menu.pl').
:- consult('enigmas.pl').
:- consult('maze.pl').

% Função principal para iniciar o jogo
start_game :-
    nl, write('===== Maze Game ====='), nl,
    main_menu.

% Função do menu principal
main_menu :-
    nl, write('1. Start New Game'), nl,
    write('2. Exit'), nl,
    write('Choose an option: '),
    read(Choice),
    handle_menu_choice(Choice).

% Lida com a escolha do menu
handle_menu_choice(1) :-
    nl, write('Starting a New Game...'), nl,
    initialize_game.

handle_menu_choice(2) :-
    nl, write('Exiting the game. Goodbye!'), nl.

handle_menu_choice(_) :-
    nl, write('Invalid option. Please choose again.'), nl,
    main_menu.

% Inicializa o estado do jogo
initialize_game :-
    initial_maze_state(MazeState),
    play_maze(MazeState).

% Joga o labirinto
play_maze(MazeState) :-
    render_maze(MazeState),
    nl, write('Enter your move (up, down, left, right): '),
    read(Move),
    move_player(Move, MazeState, NewMazeState),
    ( 
      check_exit(NewMazeState) ->
      nl, write('Congratulations! You found the exit!'), nl,
      start_enigma(NewMazeState)
    ; play_maze(NewMazeState)
    ).

% Inicia um enigma quando o jogador chega ao final do labirinto
start_enigma(MazeState) :-
    nl, write('You have reached an enigma! Solve it to continue.'), nl,
    enigma(EnigmaIndex, Question1, Question2, Options, CorrectAnswer),
    nl, write(Question1), nl, write(Question2), nl,
    write_options(Options, 1),
    read(PlayerAnswer),
    (
        PlayerAnswer =:= CorrectAnswer ->
        nl, write('Correct! You may proceed.'), nl,
        next_maze_level(MazeState)
    ;
        nl, write('Wrong answer. Try again!'), nl,
        start_enigma(MazeState)
    ).

% Escreve as opções do enigma
write_options([], _).
write_options([Option | Rest], Index) :-
    write(Index), write('. '), write(Option), nl,
    NextIndex is Index + 1,
    write_options(Rest, NextIndex).

% Passa para o próximo nível do labirinto
next_maze_level(MazeState) :-
    next_level(MazeState, NewMazeState),
    play_maze(NewMazeState).

% Função de entrada do jogo
:- start_game.
