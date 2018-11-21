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
    help/0,
    hint/0,
    dead/1,
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

draw_screen({game, GameData} = Game) ->
    draw_maze(maps:get(maze, GameData)),
    draw_hero(maps:get(hero, GameData)),
    draw_info(Game).

draw_info({game, GameData} = _Game) ->
    {hero, HeroData} = maps:get(hero, GameData),
    {stats, StatsData} = maps:get(stats, GameData),
    
    goto_xy(0, ?InfoRow),
    io:format("Turn: ~p Strength: ~p", [
        maps:get(turn, StatsData),
        maps:get(strength, HeroData)]),
    clear_eol().

draw_maze([{room, {_X, _Y, _Width, _Height}} = Room | T]) ->
    draw_room(Room),
    draw_maze(T);
draw_maze([{door, {_X, _Y}} = Door | T]) ->
    draw_door(Door),
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

draw_door({door, {X, Y}}) ->
    goto_xy(X, Y),
    io:format("#").

draw_hero({hero, Data} = _Hero) ->
    goto_xy(maps:get(position, Data)),
    io:format("@").

get_command() ->
    goto_xy(0, ?CommandRow),
    %% clear_eol/0 is needed when running from the Erlang shell,
    %% after the user inputs several characters before pressing Enter:
    clear_eol(),
    Command = io:get_chars("Command: ", 1),
    case Command of
        <<"\n">> ->
            %% Need to ignore this binary when running from the Erlang shell,
            %% where the command needs to be followed by an Enter.
            get_command();
        _ ->
            Command
    end.

clear_message() ->
    goto_xy(0, ?MessageRow),
    clear_eol(),
    ok.

welcome() ->
    clear_message(),
    io:format("Welcome to ~s, good luck! Press ? for help.", [?Name]),
    ok.

help() ->
    clear_screen(),
    goto_xy(0, 0),
    io:format("~s help~n", [?Name]),
    io:format("~n"),
    io:format("Find the treasure - ~s!~n", [?Treasure]),
    io:format("~n"),
    io:format("Commands:~n"),
    io:format("? - show this screen~n"),
    io:format("i - go North~n"),
    io:format("k - go South~n"),
    io:format("j - go West~n"),
    io:format("l - go East~n"),
    io:format("q - quit~n"),
    io:format("~n"),
    io:get_chars("Press any key to continue...", 1),
    clear_screen().
    
hint() ->
    clear_message(),
    io:format("Unknown command; press ? for help."),
    ok.

dead(Game) ->
    draw_info(Game),
    clear_message(),
    io:format("You used up all of your strength and died of exhaustion; game over.~n"),
    rip.

quit() ->
    clear_message(),
    io:format("Quitting...~n"),
    quit.
