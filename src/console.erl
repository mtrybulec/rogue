%% For tty control escape sequences, see
%% http://ascii-table.com/ansi-escape-sequences.phphttp://ascii-table.com/ansi-escape-sequences.php

-module(console).

-export([
    clear_screen/0,
    goto_xy/2,

    draw_screen/2,
    get_command/0
]).

-include("maze.hrl").

clear_screen() ->
    io:format("\033[2J").

goto_xy(X, Y) ->
    io:format("\033[~w;~wH", [Y, X]).

draw_screen(Maze, Hero) ->
    clear_screen(),
    
    draw_maze(Maze),
    draw_hero(Hero).

draw_maze([{room, [_PosX, _PosY, _Width, _Height]} = Room | T]) ->
    draw_room(Room),
    draw_maze(T);
draw_maze([]) ->
    ok.

draw_room({room, [PosX, PosY, Width, Height]}) ->
    goto_xy(PosX, PosY),
    io:format("+~s+", [lists:duplicate(Width - 2, "-")]),
    
    draw_room_walls(PosX, PosY + 1, Width, Height - 1),
    
    goto_xy(PosX, PosY + Height - 1),
    io:format("+~s+", [lists:duplicate(Width - 2, "-")]).

draw_room_walls(PosX, PosY, Width, Height) when Height > 1 ->
    goto_xy(PosX, PosY),
    io:format("|~s|", [lists:duplicate(Width - 2, ".")]),
    
    draw_room_walls(PosX, PosY + 1, Width, Height - 1);
draw_room_walls(_PosX, _PosY, _Width, _Height) ->
    ok.

draw_hero({hero, [X, Y]} = _Hero) ->
    goto_xy(X, Y),
    io:format("@").

get_command() ->
    goto_xy(0, ?ScreenHeight),
    {ok, [Command]} = io:fread("Command: ", "~s"),
    Command.
