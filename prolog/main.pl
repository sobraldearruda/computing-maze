/* Define o módulo e importa outros módulos.*/
:- module(main, [main/0]).
:- use_module(enigmas).
:- use_module(maze).

/* Predicado principal para o jogo.*/
main :-
    writeln('### COMPUTING MAZE ###'),
    enigmas(Enigmas),
    length(Enigmas, NumEnigmas),
    play_enigma_loop(0, NumEnigmas, 1),  % Adiciona um contador de labirinto (iniciando em 1)
    writeln('Congratulations! You finished all COMPUTING MAZEs!'),
    writeln('### COMPUTING MAZE ###').

/* Predicado recursivo para percorrer todos os enigmas e imprimir labirintos.*/
play_enigma_loop(CurrentIndex, TotalEnigmas, LabIndex) :-
    (CurrentIndex < TotalEnigmas ->
        initial_enigma_state(CurrentIndex, EnigmaState),
        play_single_enigma(EnigmaState, LabIndex, NextLabIndex),  % Passa o índice do labirinto
        NextIndex is CurrentIndex + 1,
        play_enigma_loop(NextIndex, TotalEnigmas, NextLabIndex)  % Atualiza o índice do labirinto
    ;
        true
    ).

/* Predicado para jogar um único enigma até acertar a resposta e imprimir o labirinto.*/
play_single_enigma(EnigmaState, LabIndex, NextLabIndex) :-
    imprimir_proximo_labirinto(LabIndex, NextLabIndex),  % Imprime o labirinto antes de mostrar o enigma
    render_enigma(EnigmaState),
    writeln('Choose an option: '),
    read_line_to_string(user_input, Option),
    (enigma_event_handler(Option, EnigmaState, UpdatedState) ->
        UpdatedState = enigma_state(_, _, _, CorrectAnswer, SelectedOption),
        (SelectedOption == CorrectAnswer ->
            writeln('Great. You are right!'), nl
        ;
            writeln('Wrong answer. Try again.'), nl,
            play_single_enigma(UpdatedState, LabIndex, NextLabIndex)
        )
    ;
        play_single_enigma(EnigmaState, LabIndex, NextLabIndex)
    ).

/* Predicado para imprimir o próximo labirinto baseado no índice atual.*/
imprimir_proximo_labirinto(LabIndex, NextLabIndex) :-
    labirinto(LabIndex, Lab),
    format('Maze ~d:', [LabIndex]), nl,
    imprimir_labirinto(Lab), nl,
    NextLabIndex is LabIndex + 1.  % Incrementa o índice para o próximo labirinto
