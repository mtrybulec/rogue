-module(maze).

-export([
    generate_maze/0,
    is_empty/3
]).

-include("maze.hrl").

generate_maze() ->
    [generate_room()].

generate_room() ->
    Width = ?MinWidth + rand:uniform(?MaxWidth - ?MinWidth),
    Height = ?MinHeight + rand:uniform(?MaxHeight - ?MinHeight),
    
    X = rand:uniform(?ScreenWidth - Width),
    Y = rand:uniform(?ScreenHeight - Height),

    {room, {X, Y, Width, Height}}.

is_empty([{room, {X, Y, Width, Height}} | _T], PosX, PosY) when
    X < PosX, Y < PosY, X + Width > PosX + 1, Y + Height > PosY + 1 ->
    true;
is_empty([_H | T], PosX, PosY) ->
    is_empty(T, PosX, PosY);
is_empty([], _PosX, _PosY) ->
    false.