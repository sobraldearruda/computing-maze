/* menu.pl.*/
:- dynamic choice/1.
:- consult('main.pl').
:- consult('maze.pl').
:- consult('enigmas.pl').

/* Exibe o menu principal.*/
main_menu :-
    writeln('* COMPUTING MAZE *'), nl,
    writeln('Choose an option:'),
    writeln('1. Play Game (press 1)'),
    writeln('2. Tutorial (press 2)'),
    writeln('3. Exit Game (press 3)'), nl,
    read_choice(Choice),
    handle_choice(Choice).

/* Predicado para ler a escolha pressionando uma tecla.*/
read_choice(Choice) :-
    get_single_char(Char),
    Char \= 10,  % Ignora a tecla Enter.
    ( Char =:= 49 -> Choice = 1;  % '1'
      Char =:= 50 -> Choice = 2;  % '2'
      Char =:= 51 -> Choice = 3;  % '3'
      writeln('Invalid option! Try again.'), nl,
      read_choice(Choice)
    ).

/* Manipula as opções do menu principal.*/
handle_choice(1) :- 
    iniciar_jogo,  % Chama a função para iniciar o jogo.
    main_menu.  % Retorna ao menu após o jogo.
handle_choice(2) :- 
    writeln('Welcome to the Tutorial:'), nl,
    writeln('Computing Maze is a 2D game for simulating mazes'),
    writeln('in the context of a Computer Science course. In each new level,'),
    writeln('you will encounter a new maze and a riddle related to a specific'),
    writeln('period of the course syllabus. Be cautious, your ultimate goal'),
    writeln('is to complete the course and successfully escape these mazes.'),
    writeln('The riddles you will solve are the key to permanently escaping'),
    writeln('each maze. But can you uncover the mysteries of the dreaded'),
    writeln('Theory of Computation? Let us find out how far you can go on'),
    writeln('this journey. To move the player, you can use the keys:'),
    writeln('W (up), A (down), S (left), D (right).'), nl,
    main_menu.  % Retorna ao menu após o tutorial.
handle_choice(3) :- 
    write('Running out of the COMPUTING MAZEs... See you!'), nl, halt.

/* Predicado para iniciar o jogo.*/
iniciar_jogo :-
    write('* COMPUTING MAZE *'), nl,
    labirintos(LabList),  % Carrega a lista de labirintos.
    jogar_labirintos(LabList).  % Joga apenas o último labirinto.
