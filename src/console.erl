-module(console).

-export([
    clear_screen/0,
    clear_eol/0,
    goto_xy/1,
    goto_xy/2,

    draw_board/1,
    draw_info/1,
    move/3,
    get_command/1,
    
    welcome/0,
    dead/1,
    done/0,
    quit/0
]).

-include("board.hrl").

-define(Name, "Yarlg").
-define(Treasure, "The Thingamajig").

-define(InfoRow, ?BoardHeight + 1).
-define(CommandRow, ?InfoRow + 1).
-define(MessageRow, ?CommandRow + 1).

-define(EmptyChar, ".").
-define(StairsChar, "=").
-define(HorizWallChar, "-").
-define(VertWallChar, "|").
-define(CornerChar, "+").
-define(DoorChar, "#").
-define(TreasureChar, "*").
-define(HeroChar, "@").

%% For tty control escape sequences, see
%% http://ascii-table.com/ansi-escape-sequences.phphttp://ascii-table.com/ansi-escape-sequences.php

clear_screen() ->
    io:format("\033[2J").

clear_eol() ->
    io:format("\033[K").

goto_xy({X, Y}) ->
    io:format("\033[~w;~wH", [Y, X]).

goto_xy(X, Y) ->
    io:format("\033[~w;~wH", [Y, X]).

draw_board(Game) ->
    draw_maze(maps:get(maze, Game)),
    draw_hero(maps:get(hero, Game)),
    draw_info(Game).

draw_info(Game) ->
    {hero, HeroData} = maps:get(hero, Game),
    Stats = maps:get(stats, Game),
    
    goto_xy(0, ?InfoRow),
    io:format("Turn: ~p Level: ~p Strength: ~p", [
        maps:get(turn, Stats),
        maps:get(level, HeroData),
        maps:get(strength, HeroData)]),
    clear_eol().

draw_maze(Maze) ->
    draw_rooms(Maze),
    draw_passages(Maze),
    draw_items(Maze),
    draw_stairs(Maze).

draw_rooms([{room, _} = Room | T]) ->
    draw_room(Room),
    draw_rooms(T);
draw_rooms([_ | T]) ->
    draw_rooms(T);
draw_rooms([]) ->
    ok.

draw_room({room, {{X1, Y1}, {X2, Y2}}}) ->
    InteriorWidth = X2 - X1 - 1,
    InteriorHeight = Y2 - Y1 - 1,
    
    draw_room_walls(X1, Y1, InteriorWidth),
    draw_room_walls(X1, Y1 + 1, InteriorWidth, InteriorHeight),
    draw_room_walls(X1, Y2, InteriorWidth).

draw_room_walls(X, Y, InteriorWidth) ->
    goto_xy(X, Y),
    io:format("~s~s~s", [
        ?CornerChar,
        lists:duplicate(InteriorWidth, ?HorizWallChar),
        ?CornerChar]).
    
draw_room_walls(X, Y, Width, Height) when Height > 0 ->
    goto_xy(X, Y),
    io:format("~s~s~s", [
        ?VertWallChar,
        lists:duplicate(Width, ?EmptyChar),
        ?VertWallChar]),
    
    draw_room_walls(X, Y + 1, Width, Height - 1);
draw_room_walls(_X, _Y, _Width, _Height) ->
    ok.

draw_passages([{door, _} = Door | T]) ->
    draw_door(Door),
    draw_passages(T);
draw_passages([{corridor, _} = Corridor | T]) ->
    draw_corridor(Corridor),
    draw_passages(T);
draw_passages([_ | T]) ->
    draw_passages(T);
draw_passages([]) ->
    ok.

draw_door({door, {X, Y}}) ->
    goto_xy(X, Y),
    io:format(?DoorChar).

draw_corridor({corridor, {{X, Y}, {X, Y}}}) ->
    goto_xy(X, Y),
    io:format(?EmptyChar);
draw_corridor({corridor, {{X1, Y1}, {X2, Y2}}}) ->
    goto_xy(X1, Y1),
    io:format(?EmptyChar),

    DeltaX = util:sign(X2 - X1),
    DeltaY = util:sign(Y2 - Y1),
    
    draw_corridor({corridor, {{X1 + DeltaX, Y1 + DeltaY}, {X2, Y2}}}).

draw_stairs([{stairs, {X, Y}} | _T]) ->
    goto_xy(X, Y),
    io:format(?StairsChar);
draw_stairs([_ | T]) ->
    draw_stairs(T);
draw_stairs([]) ->
    ok.

draw_items([{item, {X, Y}, treasure} | _T]) ->
    goto_xy(X, Y),
    io:format(?TreasureChar);
draw_items([_ | T]) ->
    draw_items(T);
draw_items([]) ->
    ok.

draw_hero({hero, Data} = _Hero) ->
    goto_xy(maps:get(position, Data)),
    io:format(?HeroChar).

move(_Maze, {X, Y}, {X, Y}) ->
    ok;
move(Maze, {OldX, OldY}, {NewX, NewY}) ->
    goto_xy(OldX, OldY),
    case maze:is_door(Maze, OldX, OldY) of
        true ->
            io:format(?DoorChar);
        false ->
            case maze:is_stairs(Maze, OldX, OldY) of
                true ->
                    io:format(?StairsChar);
                false ->
                    case maze:is_item(Maze, OldX, OldY) of
                        true ->
                            io:format(?TreasureChar);
                        false ->
                            io:format(?EmptyChar)
                    end
            end
    end,
    goto_xy(NewX, NewY),
    io:format(?HeroChar).

get_command_char() ->
    goto_xy(0, ?CommandRow),
    %% clear_eol/0 is needed when running from the Erlang shell,
    %% after the user inputs several characters before pressing Enter:
    clear_eol(),
    RawChar = io:get_chars("Command: ", 1),
    case RawChar of
        "\n" ->
            %% Need to ignore this when running from the Erlang shell,
            %% where the command needs to be followed by an Enter.
            get_command_char();
        _ ->
            RawChar
    end.

get_command(Game) ->
    CommandChar = get_command_char(),
    clear_message(),
    Command = case string:to_lower(CommandChar) of
        "c" ->
            collect_item;
        "h" ->
            show_hoard;
        "d" ->
            command_debug;
        "r" ->
            command_restart;
        "?" ->
            command_help;
        "q" ->
            command_quit;
        "i" ->
            command_move_up;
        "k" ->
            command_move_down;
        "j" ->
            command_move_left;
        "l" ->
            command_move_right;
        "t" ->
            take_stairs;
        _ ->
            command_unknown
    end,
    
    case Command of
        show_hoard ->
            hoard(Game),
            get_command(Game);
        command_debug ->
            debug(Game),
            get_command(Game);
        command_help ->
            help(Game),
            get_command(Game);
        command_unknown ->
            hint(),
            get_command(Game);
        _ ->
            {Command, string:to_upper(CommandChar) == CommandChar}
    end.

clear_message() ->
    goto_xy(0, ?MessageRow),
    clear_eol(),
    ok.

welcome() ->
    clear_message(),
    io:format("Welcome to ~s, good luck! Press ? for help.", [?Name]),
    ok.

help(Game) ->
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
    io:format("c - collect item~n"),
    io:format("h - hoard~n"),
    io:format("t - take the stairs~n"),
    io:format("q - quit~n"),
    io:format("~n"),
    io:format("Shift + a 'go' command - run (use Caps Lock to toggle run mode)~n"),
    flush_io(),
    io:get_chars("Press any key to continue...", 1),
    clear_screen(),
    draw_board(Game).

hint() ->
    clear_message(),
    io:format("Unknown command; press ? for help.").

dead(Game) ->
    draw_info(Game),
    clear_message(),
    io:format("You died of exhaustion; game over.~n").

done() ->
    clear_message(),
    io:format("Done, you won!~n").

quit() ->
    clear_message(),
    io:format("Quitting...~n").

debug(Game) ->
    clear_screen(),
    goto_xy(0, 0),
    io:format("~p~n", [Game]),
    flush_io(),
    io:get_chars("Press any key to continue...", 1),
    clear_screen(),
    draw_board(Game).

hoard(Game) ->
    {hero, HeroData} = maps:get(hero, Game),
    Items = maps:get(items, HeroData),
    
    case Items of
        [] ->
            clear_message(),
            io:format("No items in the hoard.");
        _ ->
            clear_screen(),
            goto_xy(0, 0),
            io:format("Your hoard:~n"),
            io:format("~n"),
            lists:foreach(fun(Item) -> io:format("- ~p~n", [Item]) end, Items),
            flush_io(),
            io:get_chars("Press any key to continue...", 1),
            clear_screen(),
            draw_board(Game)
    end.

flush_io() ->
    case util:is_shell() of
        true ->
            io:get_chars("", 1);
        false ->
            io:format("~n")
    end.
