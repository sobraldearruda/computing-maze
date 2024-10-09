/* Define os enigmas associados a cada labirinto.
   O formato será: enigma(LabIndex, EnigmaTexto, Opções, RespostaCorreta).*/

enigma(1, "Enigma: If you return to the start of the maze over and over again, then you are a:",
        ["1. Stack", "2. Queue", "3. Loop structure", "4. Recursion"], 3).

enigma(2, "Enigma: I am the following sequence of numbers: [0, 1, 1, 2, 3, 5, 8, 13, 21, ...]. Who am I?",
        ["1. Fibonacci sequence", "2. Arithmetic sequence", "3. Geometric sequence", "4. Taylor series"], 1).

enigma(3, "Enigma: You are the last to arrive here, but you will be the first to leave. Who are you?",
        ["1. Queue", "2. Stack", "3. Linked list", "4. Binary tree"], 2).

enigma(4, "Enigma: I have an end, but I follow parallel paths until I get there. Who am I?",
        ["1. Turing machine", "2. Deterministic finite automaton", "3. Nondeterministic finite automaton", "4. Context-free grammar"], 3).

enigma(5, "Enigma: You communicate with different platforms, acting as an intermediary between the user and the provider. Who are you?",
        ["1. API", "2. Database", "3. Operating system", "4. Middleware"], 1).

enigma(6, "Enigma: You don't know where to go, but you check all possible paths, possibly returning to the start of your journey. What technique are you?",
        ["1. Dynamic programming", "2. Backtracking", "3. Greedy", "4. Breadth-first search"], 2).

enigma(7, "Enigma: Your name is a data structure used for analysis, verification, and validation. What is your name?",
        ["1. Graph", "2. Stack", "3. Syntax tree", "4. Priority queue"], 3).

enigma(8, "Enigma: I follow a cache memory policy where you are the first to arrive and the first to leave. Who am I?",
        ["1. LIFO", "2. FIFO", "3. MRU", "4. LFU"], 2).

enigma(9, "Enigma: I am the middle, based on the beginning, upon an end. Sometimes, people call me 'reasearch'. Who am I?",
        ["1. Analysis", "2. Methodology", "3. Theoretical background", "4. References"], 2).

/* Predicado para obter o enigma associado ao labirinto.*/
initial_enigma_state(LabIndex, EnigmaState) :-
    enigma(LabIndex, Texto, Opcoes, RespostaCorreta),
    EnigmaState = enigma(Texto, Opcoes, RespostaCorreta).

/* Predicado para exibir o enigma.*/
render_enigma(enigma(Texto, Opcoes, _)) :-
    writeln(Texto), nl,
    writeln('Options:'), nl,
    forall(member(Op, Opcoes), (write(Op), nl)).

/* Predicado para verificar se a opção escolhida é válida.*/
valid_input(OptionNumber, enigma(_, _, _)) :-
    OptionNumber >= 1,
    OptionNumber =< 4.

/* Predicado para lidar com a resposta do enigma.*/
enigma_event_handler(OptionNumber, EnigmaState, UpdatedState) :-
    valid_input(OptionNumber, EnigmaState),
    UpdatedState = EnigmaState.

/* Predicado para verificar se a opção escolhida é a correta.*/
check_answer(OptionNumber, enigma(_, _, RespostaCorreta), 'Right done! You can continue now.') :-
    OptionNumber = RespostaCorreta.

check_answer(OptionNumber, enigma(_, _, RespostaCorreta), 'Wrong answer. The game is over, but you can try it all again.') :-
    OptionNumber \= RespostaCorreta.
