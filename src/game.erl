-module(game).

-export([
    play/0
]).

-include("game.hrl").

is_last_level() ->
    false.

play() ->
    Maze = maze:generate_maze(is_last_level()),
    Hero = hero:initialize_hero(Maze),
    Game = {game, #{
        maze => Maze,
        hero => Hero,
        stats => {stats, #{
            turn => 1
        }}
    }},
    
    console:clear_screen(),
    console:welcome(),
    console:draw_board(Game),
    play(Game).

play({game, GameData} = Game) ->
    {hero, HeroData} = maps:get(hero, GameData),

    console:draw_info(Game),
    {Command, Running} = console:get_command(Game),
    
    NewHero = {hero, HeroData#{running => Running}},
    NewGame = {game, GameData#{hero => NewHero}},

    case Command of
        command_restart ->
            play();
        command_quit ->
            console:quit(),
            quit;
        _ ->
            GameAfterMove = move(NewGame, Command, undefined),
            case GameAfterMove of
                rip ->
                    rip;
                _ ->
                    play(GameAfterMove)
            end
    end.

move({game, GameData} = Game, Command, undefined) ->
    Maze = maps:get(maze, GameData),
    {hero, HeroData} = maps:get(hero, GameData),
    {X, Y} = maps:get(position, HeroData),
    {DeltaX, DeltaY} = direction_to_deltas(Command),
    IsEmptyOrt1 = maze:is_empty(Maze, X + DeltaY, Y + DeltaX),
    IsEmptyOrt2 = maze:is_empty(Maze, X - DeltaY, Y - DeltaX),
    
    move(Game, Command, {IsEmptyOrt1, IsEmptyOrt2});
move({game, GameData} = _Game, Command, {IsEmptyOrt1, IsEmptyOrt2}) ->
    Hero = maps:get(hero, GameData),
    Maze = maps:get(maze, GameData),
    
    {hero, HeroData} = Hero,
    {X, Y} = maps:get(position, HeroData),
    Strength = maps:get(strength, HeroData),

    {stats, StatsData} = maps:get(stats, GameData),
    
    {DeltaX, DeltaY} = direction_to_deltas(Command),
    {NewX, NewY} = {X + DeltaX, Y + DeltaY},
    
    NewPosition = case maze:is_empty(Maze, NewX, NewY) of
        true ->
            {NewX, NewY};
        _ ->
            {X, Y}
    end,
    
    NewStrength = case maze:is_empty(Maze, NewX, NewY) of
        false ->
            %% Don't hit the wall - you'll hurt yourself!
            Strength - ?StrengthLossOnHittingWall;
        true ->
            %% Running around saps energy...
            case rand:uniform(?ReciprocalStrengthLossOnMove) of
                1 ->
                    Strength - 1;
                _ ->
                    Strength
            end
    end,
    
    NewHeroData = HeroData#{
        position => NewPosition,
        strength => NewStrength
    },
    NewGame = {game, GameData#{
        hero => {hero, NewHeroData},
        stats => {stats, StatsData#{
            turn => maps:get(turn, StatsData) + 1
        }}
    }},
    
    case NewStrength of
        0 ->
            console:dead(NewGame),
            rip;
        _ ->
            console:move(Maze, {X, Y}, NewPosition),
            case maps:get(running, HeroData) of
                true ->
                    {StopRunning, NewIsEmptyOrt1, NewIsEmptyOrt2} =
                        stop_running(Maze, NewX, NewY, DeltaX, DeltaY, IsEmptyOrt1, IsEmptyOrt2),
                    case StopRunning of
                        false ->
                            move(NewGame, Command, {NewIsEmptyOrt1, NewIsEmptyOrt2});
                        true ->
                            case restart_running_after_turn(Maze, NewX, NewY, DeltaX, DeltaY, 1) of
                                true ->
                                    NewCommand = deltas_to_direction(DeltaY, DeltaX),
                                    move(NewGame, NewCommand, undefined);
                                false ->
                                    case restart_running_after_turn(Maze, NewX, NewY, DeltaX, DeltaY, -1) of
                                        true ->
                                            NewCommand = deltas_to_direction(-DeltaY, -DeltaX),
                                            move(NewGame, NewCommand, undefined);
                                        false ->
                                            StopRunningHeroData = NewHeroData#{running => false},
                                            {game, GameData#{hero => {hero, StopRunningHeroData}}}
                                    end
                            end
                    end;
                false ->
                    NewGame
            end
    end.

direction_to_deltas(Direction) ->
    case Direction of
        command_move_up ->
            {0, -1};
        command_move_down ->
            {0, 1};
        command_move_left ->
            {-1, 0};
        command_move_right ->
            {1, 0}
    end.

deltas_to_direction(DeltaX, DeltaY) ->
    case {DeltaX, DeltaY} of
        {0, -1} ->
            command_move_up;
        {0, 1} ->
            command_move_down;
        {-1, 0} ->
            command_move_left;
        {1, 0} ->
            command_move_right
    end.

%% Stop running when:
%% - the next cell in the current direction is not empty,
%% - the next cell is next to a door,
%% - the corridor gets wider (from its narrowest point during the current run).
stop_running(Maze, X, Y, DeltaX, DeltaY, IsEmptyOrt1, IsEmptyOrt2) ->
    {NextX, NextY} = {X + DeltaX, Y + DeltaY},

    {Ort1X, Ort1Y} = {X + DeltaY, Y + DeltaX},
    {Ort2X, Ort2Y} = {X - DeltaY, Y - DeltaX},

    Result = not maze:is_empty(Maze, NextX, NextY) orelse
        maze:is_door(Maze, NextX, NextY) orelse maze:is_stairs(Maze, NextX, NextY) orelse
        maze:is_door(Maze, Ort1X, Ort1Y) orelse maze:is_stairs(Maze, Ort1X, Ort1Y) orelse
        maze:is_door(Maze, Ort2X, Ort2Y) orelse maze:is_stairs(Maze, Ort2X, Ort2Y),

    case Result of
        true ->
            {true, IsEmptyOrt1, IsEmptyOrt2};
        false ->
            NewIsEmptyOrt1 = maze:is_empty(Maze, Ort1X, Ort1Y),
            NewIsEmptyOrt2 = maze:is_empty(Maze, Ort2X, Ort2Y),
            {
                IsEmptyOrt1 < maze:is_empty(Maze, Ort1X, Ort1Y) orelse
                IsEmptyOrt2 < maze:is_empty(Maze, Ort2X, Ort2Y),
                NewIsEmptyOrt1,
                NewIsEmptyOrt2
            }
    end.

restart_running_after_turn(Maze, X, Y, DeltaX, DeltaY, TurnDirection) ->
    {NextX, NextY} = {X + DeltaX, Y + DeltaY},
    {Ort1X, Ort1Y} = {X + TurnDirection * DeltaY, Y + TurnDirection * DeltaX},
    {Ort2X, Ort2Y} = {X - TurnDirection * DeltaY, Y - TurnDirection * DeltaX},
    not maze:is_empty(Maze, NextX, NextY) andalso
        maze:is_empty(Maze, Ort1X, Ort1Y) andalso
        not maze:is_door(Maze, Ort1X, Ort1Y) andalso
        not maze:is_stairs(Maze, Ort1X, Ort1Y) andalso
        not maze:is_empty(Maze, Ort2X, Ort2Y).
