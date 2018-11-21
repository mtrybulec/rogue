-module(maze).

-export([
    generate_maze/0,
    is_empty/3,
    is_wall/3
]).

-include("maze.hrl").

generate_maze() ->
    FirstRoom = [generate_room()],
    lists:reverse(generate_maze(FirstRoom)).

generate_maze(Maze) ->
    [generate_door(Maze) | Maze].

generate_room() ->
    Width = ?MinWidth + rand:uniform(?MaxWidth - ?MinWidth),
    Height = ?MinHeight + rand:uniform(?MaxHeight - ?MinHeight),
    
    X = rand:uniform(?ScreenWidth - Width),
    Y = rand:uniform(?ScreenHeight - Height),

    {room, {X, Y, Width, Height}}.

generate_door(Maze) ->
    X = rand:uniform(?ScreenWidth),
    Y = rand:uniform(?ScreenHeight),
    
    case is_wall(Maze, X, Y) andalso
        not is_empty(Maze, X, Y) andalso
        not is_corner(Maze, X, Y) andalso
        not is_edge(X, Y) of
        true ->
            {door, {X, Y}};
        false ->
            generate_door(Maze)
    end.
    

is_empty([{room, {X, Y, Width, Height}} | _T], PosX, PosY) when
    X < PosX, Y < PosY, X + Width > PosX + 1, Y + Height > PosY + 1 ->
    true;
is_empty([{door, {X, Y}} | _T], PosX, PosY) when
    X == PosX, Y == PosY ->
    true;
is_empty([_H | T], PosX, PosY) ->
    is_empty(T, PosX, PosY);
is_empty([], _PosX, _PosY) ->
    false.

is_wall([{room, {X, Y, Width, Height}} | _T] = Maze, PosX, PosY) when
    X == PosX orelse X + Width == PosX + 1, Y =< PosY, Y + Height >= PosY + 1;
    Y == PosY orelse Y + Height == PosY + 1, X =< PosX, X + Width >= PosX + 1 ->
    not is_empty(Maze, PosX, PosY);
is_wall([_H | T], PosX, PosY) ->
    is_wall(T, PosX, PosY);
is_wall([], _PosX, _PosY) ->
    false.

is_corner([{room, {X, Y, Width, Height}} | _T], PosX, PosY) when
    X == PosX, Y == PosY;
    X + Width == PosX + 1, Y == PosY;
    X == PosX, Y + Height == PosY + 1;
    X + Width == PosX + 1, Y + Height == PosY + 1 ->
    true;
is_corner([_H | T], PosX, PosY) ->
    is_corner(T, PosX, PosY);
is_corner([], _PosX, _PosY) ->
    false.

is_edge(X, Y) when
    X == 1; Y == 1; X == ?ScreenWidth; Y == ?ScreenHeight ->
    true;
is_edge(_X, _Y) ->
    false.