%% For tty control escape sequences, see
%% http://ascii-table.com/ansi-escape-sequences.phphttp://ascii-table.com/ansi-escape-sequences.php

-module(console).

-export([
    start/0,
    
    clear_screen/0,
    goto_xy/1,
    goto_xy/2,

    draw_screen/2,
    get_command/0,
    
    quit/0
]).

-include("maze.hrl").

start() ->
    io:setopts([{binary, true}]).

clear_screen() ->
    io:format("\033[2J").

goto_xy({X, Y}) ->
    io:format("\033[~w;~wH", [Y, X]).

goto_xy(X, Y) ->
    io:format("\033[~w;~wH", [Y, X]).

draw_screen(Maze, Hero) ->
    draw_maze(Maze),
    draw_hero(Hero).

draw_maze([{room, {_X, _Y, _Width, _Height}} = Room | T]) ->
    draw_room(Room),
    draw_maze(T);
draw_maze([]) ->
    ok.

draw_room({room, {X, Y, Width, Height}}) ->
    goto_xy(X, Y),
    io:format("+~s+", [lists:duplicate(Width - 2, "-")]),
    
    draw_room_walls(X, Y + 1, Width, Height - 1),
    
    goto_xy(X, Y + Height - 1),
    io:format("+~s+", [lists:duplicate(Width - 2, "-")]).

draw_room_walls(X, Y, Width, Height) when Height > 1 ->
    goto_xy(X, Y),
    io:format("|~s|", [lists:duplicate(Width - 2, ".")]),
    
    draw_room_walls(X, Y + 1, Width, Height - 1);
draw_room_walls(_X, _Y, _Width, _Height) ->
    ok.

draw_hero({hero, Data} = _Hero) ->
    goto_xy(maps:get(position, Data)),
    io:format("@").

get_command() ->
    goto_xy(0, ?ScreenHeight),
    io:get_chars("Command: ", 1).

quit() ->
    goto_xy(0, ?ScreenHeight + 1),
    io:format("Quitting...~n"),
    ok.
