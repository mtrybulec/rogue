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
    
    PosX = rand:uniform(?ScreenWidth - Width),
    PosY = rand:uniform(?ScreenHeight - Height),

    {room, [PosX, PosY, Width, Height]}.

is_empty([{room, [PosX, PosY, Width, Height]} | _T], X, Y) when
    PosX < X, PosY < Y, PosX + Width > X + 1, PosY + Height > Y + 1 ->
    true;
is_empty([_H | T], X, Y) ->
    is_empty(T, X, Y);
is_empty([], _X, _Y) ->
    false.