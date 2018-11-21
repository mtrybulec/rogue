%% For tty control escape sequences, see
%% http://ascii-table.com/ansi-escape-sequences.phphttp://ascii-table.com/ansi-escape-sequences.php

-module(console).

-export([
    start/0,
    
    clear_screen/0,
    clear_eol/0,
    goto_xy/1,
    goto_xy/2,

    draw_screen/1,
    get_command/0,
    
    clear_message/0,
    welcome/0,
    hint/0,
    dead/0,
    quit/0
]).

-include("maze.hrl").

start() ->
    io:setopts([{binary, true}]).

clear_screen() ->
    io:format("\033[2J").

clear_eol() ->
    io:format("\033[K").

goto_xy({X, Y}) ->
    io:format("\033[~w;~wH", [Y, X]).

goto_xy(X, Y) ->
    io:format("\033[~w;~wH", [Y, X]).

draw_screen({game, GameData} = _Game) ->
    Maze = maps:get(maze, GameData),
    Hero = maps:get(hero, GameData),
    {hero, HeroData} = Hero,
    {stats, StatsData} = maps:get(stats, GameData),
    
    draw_maze(Maze),
    draw_hero(Hero),

    goto_xy(0, ?InfoRow),
    io:format("Turn: ~p Strength: ~p", [
        maps:get(turn, StatsData),
        maps:get(strength, HeroData)]),
    clear_eol().

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
    goto_xy(0, ?CommandRow),
    %% clear_eol/0 is needed when running from the Erlang shell,
    %% after the user inputs several characters before pressing Enter:
    clear_eol(),
    io:get_chars("Command: ", 1).

clear_message() ->
    goto_xy(0, ?MessageRow),
    clear_eol(),
    ok.

welcome() ->
    clear_message(),
    io:format("Welcome to ~s, good luck! Press ? for help.", [?Name]),
    ok.

hint() ->
    clear_message(),
    io:format("Unknown command; press ? for help."),
    ok.

dead() ->
    clear_message(),
    io:format("You've used up all of your strength and died; game over.~n"),
    rip.

quit() ->
    clear_message(),
    io:format("Quitting...~n"),
    quit.
