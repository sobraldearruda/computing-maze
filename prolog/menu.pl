:- module(menu, [
    start_menu/0,
    menu_state/1,
    render_menu/1,
    menu_event_handler/3,
    menu_time_handler/3
]).

% Define os diferentes estados possíveis do menu
menu_state(main_menu).
menu_state(play_game).
menu_state(tutorial).
menu_state(exit_game).

% Mostra a interface do menu com base no estado atual
render_menu(main_menu) :-
    writeln('* COMPUTING MAZE *'),
    writeln('1. Play Game'),
    writeln('2. Tutorial'),
    writeln('3. Exit Game').

render_menu(play_game) :-
    writeln('Iniciando o jogo...').

render_menu(tutorial) :-
    writeln('Welcome to the Tutorial:'),
    writeln('Computing Maze is a 2D game for simulating mazes'),
    writeln('in the context of a Computer Science course. In each new level,'),
    writeln('you will encounter a new maze and a riddle related to a specific'),
    writeln('period of the course syllabus. Be cautious, your ultimate goal'),
    writeln('is to complete the course and successfully escape these mazes.'),
    writeln('The riddles you will solve are the key to permanently escaping'),
    writeln('each maze. But can you uncover the mysteries of the dreaded'),
    writeln('Theory of Computation? Let\'s find out how far you can go on'),
    writeln('this journey. To move the player, you can use the standard'),
    writeln('game method using the arrow keys: Up, Down, Right, Left.'),
    writeln('Press 4 to return to the main menu.').

render_menu(exit_game) :-
    writeln('Saindo do jogo...').

% Manipula eventos do menu com base na opção escolhida pelo usuário
menu_event_handler('1', _, play_game).
menu_event_handler('2', _, tutorial).
menu_event_handler('3', _, exit_game).
menu_event_handler('4', tutorial, main_menu).
menu_event_handler(_, CurrentState, CurrentState).

% Manipula o tempo no menu (neste caso, sem efeito)
menu_time_handler(_, State, State).

% Função principal para iniciar o menu
start_menu :-
    menu_loop(main_menu).

% Loop principal do menu
menu_loop(State) :-
    render_menu(State),
    read_line_to_string(user_input, Input),
    menu_event_handler(Input, State, NewState),
    (NewState = exit_game ->
        render_menu(exit_game);
        menu_loop(NewState)).
