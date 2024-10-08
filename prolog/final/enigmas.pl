% Define os enigmas associados a cada labirinto
% O formato será: enigma(LabIndex, EnigmaTexto, Opções, RespostaCorreta).

enigma(1, "Se você volta ao início do labirinto repetidamente, então você é um(a): ",
        ["1.Pilha", "2.Fila", "3.Estrutura de laço", "4.Recursão"], 3).

enigma(2, "Sou a seguinte sequência de números: [0, 1, 1, 2, 3, 5, 8, 13, 21, ...]. Quem sou eu?",
        ["1.Sequência de Fibonacci", "2.Sequência aritmética", "3.Sequência geométrica", "4.Série de Taylor"], 1).

enigma(3, "Você é o último a chegar aqui, mas será o primeiro a sair. Quem é você?",
        ["1.Fila", "2.Pilha", "3.Lista encadeada", "4.Árvore binária"], 2).

enigma(4, "Tenho um fim, mas sigo caminhos paralelos até chegar lá. Quem sou eu?",
        ["1.Máquina de Turing", "2.Autômato finito determinístico", "3.Autômato finito não determinístico", "4.Gramática livre de contexto"], 3).

enigma(5, "Você se comunica com diferentes plataformas, agindo como um intermediário entre o usuário e o provedor. Quem é você?",
        ["1.API", "2.Banco de dados", "3.Sistema operacional", "4.Middleware"], 1).

enigma(6, "Você não sabe para onde ir, mas verifica todos os caminhos possíveis, possivelmente voltando ao início da sua jornada. Que técnica é você?",
        ["1.Programação dinâmica", "2.Backtracking", "3.Guloso", "4.Busca em largura"], 2).

enigma(7, "Seu nome é uma estrutura de dados usada para análise, verificação e validação. Qual é o seu nome?",
        ["1.Grafo", "2.Pilha", "3.Árvore sintática", "4.Fila de prioridade"], 3).

enigma(8, "Eu sigo uma política de memória cache onde você é o primeiro a chegar e o primeiro a sair. Quem sou eu?",
        ["1.LIFO", "2.FIFO", "3.MRU", "4.LFU"], 2).

enigma(9, "Eu sou o meio, com base no início, ao final de um caminho. Às vezes, me chamam de 'research'. Quem sou eu?",
        ["1.Análise", "2.Metodologia", "3.Referencial teórico", "4.Referências"], 2).

% Função para obter o enigma associado ao labirinto
initial_enigma_state(LabIndex, EnigmaState) :-
    enigma(LabIndex, Texto, Opcoes, RespostaCorreta),
    EnigmaState = enigma(Texto, Opcoes, RespostaCorreta).

% Função para exibir o enigma
render_enigma(enigma(Texto, Opcoes, _)) :-
    write(Texto), nl,
    write('Opções:'), nl,
    forall(member(Op, Opcoes), (write(Op), nl)).

% Função para verificar se a opção escolhida é válida
valid_input(OptionNumber, enigma(_, _, _)) :-
    OptionNumber >= 1,
    OptionNumber =< 4.

% Função para lidar com a resposta do enigma
enigma_event_handler(OptionNumber, EnigmaState, UpdatedState) :-
    valid_input(OptionNumber, EnigmaState),
    UpdatedState = EnigmaState.

% Função para verificar se a opção escolhida é a correta
check_answer(OptionNumber, enigma(_, _, RespostaCorreta), 'Resposta correta! Você pode prosseguir.') :-
    OptionNumber = RespostaCorreta.

check_answer(OptionNumber, enigma(_, _, RespostaCorreta), 'Resposta incorreta. O jogo acabou.') :-
    OptionNumber \= RespostaCorreta.
