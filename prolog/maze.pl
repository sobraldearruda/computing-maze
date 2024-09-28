% Define o módulo maze com seus predicados
:- module(maze, [
    maze/3,
    scale_factor/1,
    cell_size/1,
    calculate_maze_offset_x/2,
    calculate_maze_offset_y/2,
    draw_maze/3,
    is_walkable/4,
    maze_data/3
]).

% Fator de escala aplicado ao tamanho das células do labirinto
scale_factor(0.7).

% Define o tamanho de cada célula do labirinto após a aplicação do fator de escala
cell_size(Size) :-
    scale_factor(Factor),
    Size is 40 * Factor.

% Predicado para representar um labirinto com uma matriz de inteiros (grid), largura e altura
maze(Grid, Width, Height) :-
    length(Grid, Height),
    Grid = [Row | _],
    length(Row, Width).

% Calcula o deslocamento horizontal e vertical necessários para centralizar o labirinto na tela
calculate_maze_offset_x(ScreenWidth, OffsetX) :-
    cell_size(CellSize),
    OffsetX is - (ScreenWidth / 2) + 2.5 * CellSize.

calculate_maze_offset_y(ScreenHeight, OffsetY) :-
    cell_size(CellSize),
    OffsetY is (ScreenHeight / 2) - (1.5 * CellSize).

% Desenha o labirinto no terminal
draw_maze(Maze, MazeOffsetX, MazeOffsetY) :-
    maze(Grid, _, _),
    draw_cells(Grid, 0, MazeOffsetX, MazeOffsetY).

draw_cells([], _, _, _).
draw_cells([Row | Rows], Y, MazeOffsetX, MazeOffsetY) :-
    draw_row(Row, 0, Y, MazeOffsetX, MazeOffsetY),
    Y1 is Y + 1,
    draw_cells(Rows, Y1, MazeOffsetX, MazeOffsetY).

draw_row([], _, _, _, _).
draw_row([Cell | Cells], X, Y, MazeOffsetX, MazeOffsetY) :-
    cell_size(CellSize),
    PosX is MazeOffsetX + X * CellSize,
    PosY is MazeOffsetY - Y * CellSize,
    draw_cell(Cell, PosX, PosY),
    X1 is X + 1,
    draw_row(Cells, X1, Y, MazeOffsetX, MazeOffsetY).

draw_cell(0, X, Y) :-
    format("Paredes em (~w, ~w)\n", [X, Y]).
draw_cell(1, X, Y) :-
    format("Caminho em (~w, ~w)\n", [X, Y]).
draw_cell(2, X, Y) :-
    format("Saída em (~w, ~w)\n", [X, Y]).

% Verifica se uma determinada posição no labirinto é caminhável
is_walkable(Maze, X, Y, Walkable) :-
    maze(Grid, Width, Height),
    cell_size(CellSize),
    GridX is floor((X + Width * CellSize / 2) / CellSize),
    GridY is floor((Height * CellSize / 2 - Y) / CellSize),
    GridX >= 0, GridX < Width,
    GridY >= 0, GridY < Height,
    nth0(GridY, Grid, Row),
    nth0(GridX, Row, Cell),
    (Cell =:= 1 ; Cell =:= 2),
    Walkable = true.

is_walkable(_, _, _, false).

% Lista de labirintos predefinidos
maze_data(1, [[0, 0, 0, 0, 0], [0, 1, 1, 2, 0], [0, 1, 0, 1, 0], [0, 1, 1, 1, 0], [0, 0, 0, 0, 0]], 5).
maze_data(2, [[0, 0, 0, 0, 0], [0, 1, 1, 2, 0], [0, 0, 0, 1, 0], [0, 1, 1, 1, 0], [0, 0, 0, 0, 0]], 5).

% Exemplo de como obter um labirinto
load_maze(Id, Maze) :-
    maze_data(Id, Grid, Width),
    Height is length(Grid),
    Maze = maze(Grid, Width, Height).

% Exemplo de uso
:- load_maze(1, Maze),
   calculate_maze_offset_x(800, OffsetX),
   calculate_maze_offset_y(600, OffsetY),
   draw_maze(Maze, OffsetX, OffsetY),
   is_walkable(Maze, 1.0, 1.0, Walkable),
   format("É caminhável: ~w", [Walkable]).
