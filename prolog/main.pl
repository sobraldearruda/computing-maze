/* Definindo o módulo main e importando enigmas.*/
:- module(main, [main/0]).
:- use_module(enigmas).

/* Função principal para jogar o jogo dos enigmas.*/
main :-
    writeln('Bem-vindo ao Jogo dos Enigmas!'),
    enigmas(Enigmas),
    length(Enigmas, NumEnigmas),
    play_enigma_loop(0, NumEnigmas),
    writeln('Parabéns! Você concluiu todos os enigmas!').

/* Função recursiva para percorrer todos os enigmas.*/
play_enigma_loop(CurrentIndex, TotalEnigmas) :-
    (CurrentIndex < TotalEnigmas ->
        initial_enigma_state(CurrentIndex, EnigmaState),
        play_single_enigma(EnigmaState),
        NextIndex is CurrentIndex + 1,
        play_enigma_loop(NextIndex, TotalEnigmas)
    ;
        true
    ).

/* Função para jogar um único enigma até acertar a resposta.*/
play_single_enigma(EnigmaState) :-
    render_enigma(EnigmaState),
    writeln('Escolha uma opção:'),
    read_line_to_string(user_input, Option),
    (enigma_event_handler(Option, EnigmaState, UpdatedState) ->
        % Caso a opção seja válida
        UpdatedState = enigma_state(_, _, _, CorrectAnswer, SelectedOption),
        (SelectedOption == CorrectAnswer ->
            writeln('Resposta correta!'), nl
        ;
            writeln('Resposta incorreta. Tente novamente.'), nl,
            play_single_enigma(UpdatedState)
        )
    ;
        % Se a opção for inválida, exibe a mensagem e repete o enigma
        play_single_enigma(EnigmaState)
    ).
