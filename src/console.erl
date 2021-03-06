-module(console).

-export([
    clear_screen/0,
    clear_eol/0,
    goto_xy/1,
    goto_xy/2,

    draw_board/1,
    draw_info/1,
    update/3,
    move_hero/3,
    move_monster/4,
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
    Hero = maps:get(hero, Game),
    Stats = maps:get(stats, Game),
    
    goto_xy(0, ?InfoRow),
    io:format("Turn: ~p Level: ~p Strength: ~p", [
        maps:get(turn, Stats),
        maps:get(level, Hero),
        maps:get(strength, Hero)]),
    clear_eol().

draw_maze(Maze) ->
    draw_rooms(Maze),
    draw_passages(Maze),
    draw_stairs(Maze),
    draw_items(Maze),
    draw_monsters(Maze).

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

draw_monsters([{monster, {X, Y}, {Monster, _Strength}} | T]) ->
    goto_xy(X, Y),
    MonsterChar = case Monster of
        goblin ->
            "G";
        orc ->
            "O"
    end,
    io:format(MonsterChar),
    draw_monsters(T);
draw_monsters([_ | T]) ->
    draw_monsters(T);
draw_monsters([]) ->
    ok.

draw_hero(Hero) ->
    goto_xy(maps:get(position, Hero)),
    io:format(?HeroChar).

update(Maze, X, Y) ->
    goto_xy(X, Y),
    case maze:is_monster(Maze, X, Y) of
        true ->
            Monster = maze:get_monster(Maze, X, Y),
            draw_monsters([Monster]);
        false ->
            case maze:is_door(Maze, X, Y) of
                true ->
                    io:format(?DoorChar);
                false ->
                    case maze:is_stairs(Maze, X, Y) of
                        true ->
                            io:format(?StairsChar);
                        false ->
                            case maze:is_item(Maze, X, Y) of
                                true ->
                                    io:format(?TreasureChar);
                                false ->
                                    io:format(?EmptyChar)
                            end
                    end
            end
    end.

move_hero(_Maze, {X, Y}, {X, Y}) ->
    ok;
move_hero(Maze, {OldX, OldY}, {NewX, NewY}) ->
    update(Maze, OldX, OldY),
    goto_xy(NewX, NewY),
    io:format(?HeroChar).

move_monster(_Maze, {X, Y}, {X, Y}, _MonsterType) ->
    ok;
move_monster(Maze, {OldX, OldY}, {NewX, NewY}, MonsterType) ->
    update(Maze, OldX, OldY),
    draw_monsters([{monster, {NewX, NewY}, {MonsterType, undefined}}]).

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
        "b" ->
            show_board;
        "d" ->
            show_debug_info;
        "r" ->
            restart_game;
        "?" ->
            show_help;
        "q" ->
            quit_game;
        "i" ->
            move_north;
        "k" ->
            move_south;
        "j" ->
            move_west;
        "l" ->
            move_east;
        "t" ->
            take_stairs;
        _ ->
            unknown_command
    end,
    
    case Command of
        show_board ->
            clear_screen(),
            draw_board(Game),
            get_command(Game);
        show_hoard ->
            hoard(Game),
            get_command(Game);
        show_debug_info ->
            debug(Game),
            get_command(Game);
        show_help ->
            help(Game),
            get_command(Game);
        unknown_command ->
            hint(),
            get_command(Game);
        _ ->
            {Command, string:to_upper(CommandChar) == CommandChar}
    end.

clear_message() ->
    goto_xy(0, ?MessageRow),
    clear_eol().

show_message(Message) ->
    clear_message(),
    io:format(Message).

welcome() ->
    show_message(io_lib:format("Welcome to ~s, good luck! Press ? for help.", [?Name])).

help(Game) ->
    show_info_screen(
        Game,
        io_lib:format("~s help", [?Name]),
        fun () ->
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
            io:format("Shift + a 'go' command - run (use Caps Lock to toggle run mode)~n")
        end
    ).

hint() ->
    show_message("Unknown command; press ? for help.").

dead(Game) ->
    draw_info(Game),
    show_message("You died of exhaustion; game over.~n").

done() ->
    show_message(io_lib:format("Done, good job, you recovered ~s!~n", [?Treasure])).

quit() ->
    show_message("Quitting...~n").

debug(Game) ->
    show_info_screen(
        Game,
        "Debug information",
        fun () ->
            io:format("~p~n", [Game])
        end
    ).

hoard(Game) ->
    Hero = maps:get(hero, Game),
    Items = maps:get(items, Hero),
    
    case Items of
        [] ->
            show_message("No items in the hoard.");
        _ ->
            show_info_screen(
                Game,
                "Your hoard",
                fun () ->
                    lists:foreach(
                        fun(Item) ->
                            io:format("- ~p~n", [Item])
                        end,
                        Items)
                end
            )
    end.

flush_io() ->
    case util:is_shell() of
        true ->
            io:get_chars("", 1);
        false ->
            io:format("~n")
    end.

show_info_screen(Game, Title, F) when is_function(F, 0) ->
    clear_screen(),
    goto_xy(0, 0),
    io:format("~s:~n", [Title]),
    io:format("~n"),
    F(),
    flush_io(),
    io:get_chars("Press any key to continue...", 1),
    clear_screen(),
    draw_board(Game).
    