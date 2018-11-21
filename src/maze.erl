-module(maze).

-export([
    generate_maze/0,
    draw_maze/1,
    
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

draw_maze(Maze) ->
    console:clear_screen(),
    draw_rooms(Maze),
    
    {hero, [X, Y]} = hero:initialize_hero(Maze),
    console:goto_xy(X, Y),
    io:format("@"),
    
    console:goto_xy(0, ?ScreenHeight),
    io:format("").

draw_rooms([{room, [_PosX, _PosY, _Width, _Height]} = Room | T]) ->
    draw_room(Room),
    draw_rooms(T);
draw_rooms([]) ->
    ok.

draw_room({room, [PosX, PosY, Width, Height]}) ->
    console:goto_xy(PosX, PosY),
    io:format("+~s+", [lists:duplicate(Width - 2, "-")]),

    draw_room_walls(PosX, PosY + 1, Width, Height - 1),
    
    console:goto_xy(PosX, PosY + Height - 1),
    io:format("+~s+", [lists:duplicate(Width - 2, "-")]).

draw_room_walls(PosX, PosY, Width, Height) when Height > 1 ->
    console:goto_xy(PosX, PosY),
    io:format("|~s|", [lists:duplicate(Width - 2, ".")]),

    draw_room_walls(PosX, PosY + 1, Width, Height - 1);
draw_room_walls(_PosX, _PosY, _Width, _Height) ->
    ok.

is_empty([{room, [PosX, PosY, Width, Height]} | _T], X, Y) when
    PosX < X, PosY < Y, PosX + Width > X + 1, PosY + Height > Y + 1 ->
    true;
is_empty([_H | T], X, Y) ->
    is_empty(T, X, Y);
is_empty([], _X, _Y) ->
    false.