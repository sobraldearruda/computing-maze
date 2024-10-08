% menu.pl
:- dynamic choice/1.
:- consult('main.pl').
:- consult('maze.pl').
:- consult('enigmas.pl').

main_menu :-
    write('Escolha uma opção:'), nl,
    write('1. Jogar (pressione 1)'), nl,
    write('2. Tutorial (pressione 2)'), nl,
    write('3. Sair (pressione 3)'), nl,
    read_choice(Choice),
    handle_choice(Choice).

% Função para ler a escolha pressionando uma tecla
read_choice(Choice) :-
    get_single_char(Char),
    Char \= 10,  % Ignora a tecla Enter
    ( Char =:= 49 -> Choice = 1;  % '1'
      Char =:= 50 -> Choice = 2;  % '2'
      Char =:= 51 -> Choice = 3;  % '3'
      write('Escolha inválida! Tente novamente.'), nl,
      read_choice(Choice)
    ).

handle_choice(1) :- 
    iniciar_jogo,  % Chama a função para iniciar o jogo
    main_menu.  % Retorna ao menu após o jogo
handle_choice(2) :- 
    writeln('Welcome to the Tutorial:'),
    writeln('Computing Maze é um jogo 2D para simular labirintos'),
    writeln('no contexto de um curso de Ciência da Computação. Em cada novo nível,'),
    writeln('você encontrará um novo labirinto e um enigma relacionado a um período específico'),
    writeln('do currículo do curso. Tenha cautela, seu objetivo final'),
    writeln('é completar o curso e escapar com sucesso destes labirintos.'),
    writeln('Os enigmas que você resolver são a chave para escapar permanentemente'),
    writeln('de cada labirinto. Mas você consegue desvendar os mistérios da temida'),
    writeln('Teoria da Computação? Vamos descobrir até onde você pode ir nesta'),
    writeln('jornada. Para mover o jogador, você pode usar os métodos padrão'),
    writeln('utilizando as teclas W A S D.'),
    main_menu.  % Retorna ao menu após o tutorial
handle_choice(3) :- 
    write('Saindo do jogo. Até mais!'), nl, halt.

% Função para iniciar o jogo
iniciar_jogo :-
    write('Bem-vindo ao Computing Maze - Última Fase!'), nl,
    labirintos(LabList),  % Carrega a lista de labirintos
    jogar_labirintos(LabList).  % Joga apenas o último labirinto

