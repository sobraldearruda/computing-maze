i :- initialization(main).
:- consult('maze.pl').  % Carrega o arquivo com os labirintos.
:- consult('enigmas.pl').  % Carrega o arquivo com os enigmas.

/* Lista de labirintos.*/
labirintos([1, 2, 3, 4, 5, 6, 7, 8, 9]).

/* Posição inicial do jogador.*/
start_pos(2, 2).

/* Inicia o jogo.*/
main :-
    main_menu.  % Inicia o menu principal.

/* Predicado para jogar os labirintos sequencialmente.*/
jogar_labirintos([]) :-
    writeln('Congratulations! You finished all COMPUTING MAZEs!'), nl.

jogar_labirintos([LabIndex | Rest]) :-  % Joga o labirinto atual.
    maze(LabIndex, Maze),
    start_pos(StartX, StartY),  % Usa a posição inicial.
    play(Maze, StartX, StartY, Rest, LabIndex).  % Posição inicial.

/* Limpa a tela (simula o comando de limpar terminal).*/
clear_screen :-
    write('\e[H\e[2J').  % Sequência ANSI para limpar a tela pelo terminal.

/* Predicado principal do jogo.*/
play(Maze, X, Y, Remaining, LabIndex) :-
    clear_screen,  % Limpa a tela antes de mostrar o novo estado de um labirinto.
    display_maze(Maze, X, Y),
    writeln('Press the key to move (w: up, s: down, a: left, d: right): '), nl,
    input_char(Move),
    move(Move, X, Y, NewX, NewY),
    (is_exit(NewX, NewY, Maze) ->  % Verifica se encontrou a saída.
        writeln('Congratulations! You found the way out!'), nl,
        resolver_enigma(LabIndex, Remaining),  % Chama o predicado para resolver o enigma associado ao labirinto.
        sleep(0.5),
        jogar_labirintos(Remaining)  % Jogar com o próximo labirinto.
    ;
        (within_bounds(NewX, NewY, Maze), valid_move(NewX, NewY, Maze) ->
            move_feedback(Move),
            play(Maze, NewX, NewY, Remaining, LabIndex)
        ;
            writeln('Invalid movement or out of bounds! Try again.'), nl,
            play(Maze, X, Y, Remaining, LabIndex)
        )
    ).

/* Valida um movimento.*/
valid_move(X, Y, Maze) :-
    nth1(Y, Maze, Row),
    nth1(X, Row, Cell),
    Cell \= 0.  % Permite o movimento se não for uma parede (0).

/* Valida se o movimento está dentro dos limites do labirinto.*/
within_bounds(X, Y, Maze) :-
    length(Maze, NumRows),
    nth1(1, Maze, FirstRow),
    length(FirstRow, NumCols),
    X > 0, X =< NumCols,
    Y > 0, Y =< NumRows.

/* Movimentos do jogador.*/
move(w, X, Y, X, NewY) :- NewY is Y - 1. % cima.
move(s, X, Y, X, NewY) :- NewY is Y + 1. % baixo.
move(a, X, Y, NewX, Y) :- NewX is X - 1. % esquerda.
move(d, X, Y, NewX, Y) :- NewX is X + 1. % direita.

/* Exibe feedback de movimento.*/
move_feedback(w) :- write('You have moved up.'), nl.
move_feedback(s) :- write('You have moved down.'), nl.
move_feedback(a) :- write('You have moved to the left.'), nl.
move_feedback(d) :- write('You have moved to the right.'), nl.

/* Verifica se a posição é a saída.*/
is_exit(X, Y, Maze) :-
    nth1(Y, Maze, Row),
    nth1(X, Row, Cell),
    Cell = 2.  % A saída é representada pelo valor 2.

/* Exibe o labirinto.*/
display_maze(Maze, X, Y) :-
    nl,
    display_rows(Maze, X, Y, 1).

/* Exibe as linhas do labirinto.*/
display_rows([], _, _, _).
display_rows([Row | Rest], X, Y, RowNum) :-
    display_columns(Row, X, Y, RowNum, 1),
    nl,
    RowNum1 is RowNum + 1,
    display_rows(Rest, X, Y, RowNum1).

/* Exibe as colunas do labirinto.*/
display_columns([], _, _, _, _).
display_columns([C | Rest], X, Y, RowNum, ColNum) :-
    (RowNum = Y, ColNum = X ->
        write('P')  % Posição do jogador.
    ;
        (C = 0 -> write('#') ; (C = 2 -> write('S') ; write(' ')))
    ),
    ColNum1 is ColNum + 1,
    display_columns(Rest, X, Y, RowNum, ColNum1).

/* Obtém o caractere de entrada e faz a validação.*/
input_char(Move) :-
    get_single_char(Char),
    char_code(Move, Char),
    member(Move, [w, a, s, d]), !.
input_char(Move) :-
    write('Invalid input! Use w, a, s, d to move.'), nl,
    input_char(Move).

/* Predicado para capturar a resposta numérica do jogador para o enigma.*/
input_number(Number) :-
    read_line_to_string(user_input, Input),  % Lê a entrada como string.
    number_string(Number, Input),  % Converte a string para número.
    between(1, 4, Number).  % Verifica se o número está entre 1 e 4.

/* Predicado para resolver o enigma associado ao labirinto.*/
resolver_enigma(LabIndex, Remaining) :-
    initial_enigma_state(LabIndex, EnigmaState),  % Inicializa o enigma com base no índice do labirinto.
    render_enigma(EnigmaState),  % Exibe o enigma para o jogador.
    write('\n'),
    writeln('Enter the number of your option: '), nl,
    (input_number(OptionNumber) ->  % Recebe a entrada numérica do usuário.
        (valid_input(OptionNumber, EnigmaState) ->  % Verifica se a entrada é válida.
            enigma_event_handler(OptionNumber, EnigmaState, UpdatedState),
            % Verifica a resposta e pega a mensagem.
            check_answer(OptionNumber, UpdatedState, MensagemResposta),
            write(MensagemResposta), nl,
            (MensagemResposta = 'Right done! You can continue now.' ->
                (Remaining = [] ->  % Se não há mais labirintos restantes, o jogo termina.
                    writeln('Congratulations! You finished all COMPUTING MAZEs!'), nl, halt  % Mensagem de vitória.
                ;
                    sleep(0.5),
                    jogar_labirintos(Remaining)  % Continua com o próximo labirinto.
                )
            ;
                halt  % Termina a execução do jogo.
            )
        ;
            writeln('Invalid option! Try again.'), nl,
            resolver_enigma(LabIndex, Remaining)  % Recomeça o processo se a entrada for inválida.
        )
    ;
        writeln('Invalid input! Try again.'), nl,
        resolver_enigma(LabIndex, Remaining)
    ).
