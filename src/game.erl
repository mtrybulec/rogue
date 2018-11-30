-module(game).

-export([
    play/0
]).

-define(LevelCount, 3).
-define(ReciprocalStrengthLossOnMove, 10).
-define(StrengthLossOnHittingWallOrGround, 1).

is_last_level(Level) ->
    Level >= ?LevelCount.

play() ->
    Maze = maze:generate_maze(is_last_level(1)),
    Hero = hero:initialize_hero(Maze),
    Game = #{
        maze => Maze,
        hero => Hero,
        stats => #{
            turn => 1
        }
    },
    
    console:clear_screen(),
    console:welcome(),
    console:draw_board(Game),
    
    play(Game).

play(Game) ->
    {hero, HeroData} = maps:get(hero, Game),
    Items = maps:get(items, HeroData),
    
    case lists:any(fun(Item) -> Item == treasure end, Items) of
        true ->
            console:done(),
            done;
        false ->
            Strength = maps:get(strength, HeroData),
    
            case Strength =< 0 of
                true ->
                    console:dead(Game),
                    rip;
                false ->
                    console:draw_info(Game),
                    {Command, Running} = console:get_command(Game),
            
                    case Command of
                        command_restart ->
                            play();
                        command_quit ->
                            console:quit(),
                            quit;
                        collect_item ->
                            NewGame = collect_item(Game),
                            play(NewGame);
                        take_stairs ->
                            NewGame = take_stairs(Game),
                            play(NewGame);
                        _ ->
                            NewGame = move(Game, Command, Running, undefined),
                            play(NewGame)
                    end
            end
    end.

collect_item(Game) ->
    {hero, HeroData} = maps:get(hero, Game),
    Stats = maps:get(stats, Game),
    NewStats = Stats#{
        turn => maps:get(turn, Stats) + 1
    },
    Maze = maps:get(maze, Game),
    {X, Y} = maps:get(position, HeroData),
    Strength = maps:get(strength, HeroData),
    
    case maze:is_item(Maze, X, Y) of
        true ->
            Items = maps:get(items, HeroData),
            {Item, NewMaze} = maze:remove_item(Maze, X, Y),
    
            NewStrength = case rand:uniform(?ReciprocalStrengthLossOnMove) of
                1 ->
                    Strength - 1;
                _ ->
                    Strength
            end,
    
            Game#{
                maze => NewMaze,
                hero => {hero, HeroData#{
                    items => [Item] ++ Items,
                    strength => NewStrength}},
                stats => NewStats
            };
        false ->
            NewStrength = Strength - ?StrengthLossOnHittingWallOrGround,
            NewHeroData = HeroData#{strength => NewStrength},
            
            Game#{
                hero => {hero, NewHeroData},
                stats => NewStats
            }
    end.

take_stairs(Game) ->
    {hero, HeroData} = maps:get(hero, Game),
    Stats = maps:get(stats, Game),
    NewStats = Stats#{
        turn => maps:get(turn, Stats) + 1
    },
    Maze = maps:get(maze, Game),
    {X, Y} = maps:get(position, HeroData),
    Strength = maps:get(strength, HeroData),
    
    case maze:is_stairs(Maze, X, Y) of
        true ->
            Level = maps:get(level, HeroData),
            NewLevel = Level + 1,
            NewMaze = maze:generate_maze(is_last_level(NewLevel)),
            Position = hero:initialize_hero_position(NewMaze),
            
            NewStrength = case rand:uniform(?ReciprocalStrengthLossOnMove) of
                1 ->
                    Strength - 1;
                _ ->
                    Strength
            end,
            
            NewGame = Game#{
                maze => NewMaze,
                hero => {hero, HeroData#{
                    level => NewLevel,
                    position => Position,
                    strength => NewStrength}},
                stats => NewStats
            },
            
            console:clear_screen(),
            console:draw_board(NewGame),
            
            NewGame;
        false ->
            NewStrength = Strength - ?StrengthLossOnHittingWallOrGround,
            NewHeroData = HeroData#{strength => NewStrength},
            
            Game#{
                hero => {hero, NewHeroData},
                stats => NewStats
            }
    end.

move(Game, Command, Running, undefined) ->
    Maze = maps:get(maze, Game),
    {hero, HeroData} = maps:get(hero, Game),
    {X, Y} = maps:get(position, HeroData),
    {DeltaX, DeltaY} = direction_to_deltas(Command),
    IsEmptyOrt1 = maze:is_empty(Maze, X + DeltaY, Y + DeltaX),
    IsEmptyOrt2 = maze:is_empty(Maze, X - DeltaY, Y - DeltaX),
    
    move(Game, Command, Running, {IsEmptyOrt1, IsEmptyOrt2});
move(Game, Command, Running, {IsEmptyOrt1, IsEmptyOrt2}) ->
    Hero = maps:get(hero, Game),
    Maze = maps:get(maze, Game),
    
    {hero, HeroData} = Hero,
    {X, Y} = maps:get(position, HeroData),
    Strength = maps:get(strength, HeroData),

    Stats = maps:get(stats, Game),
    
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
            Strength - ?StrengthLossOnHittingWallOrGround;
        true ->
            %% Walking around saps energy; running even more so...
            case rand:uniform(?ReciprocalStrengthLossOnMove div (util:boolean_to_integer(Running) + 1)) of
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
    NewGame = Game#{
        hero => {hero, NewHeroData},
        stats => Stats#{
            turn => maps:get(turn, Stats) + 1
        }
    },
    
    case NewStrength of
        0 ->
            NewGame;
        _ ->
            console:move(Maze, {X, Y}, NewPosition),
            case Running of
                true ->
                    {StopRunning, NewIsEmptyOrt1, NewIsEmptyOrt2} =
                        stop_running(Maze, NewX, NewY, DeltaX, DeltaY, IsEmptyOrt1, IsEmptyOrt2),
                    case StopRunning of
                        false ->
                            move(NewGame, Command, true, {NewIsEmptyOrt1, NewIsEmptyOrt2});
                        true ->
                            case restart_running_after_turn(Maze, NewX, NewY, DeltaX, DeltaY, 1) of
                                true ->
                                    NewCommand = deltas_to_direction(DeltaY, DeltaX),
                                    move(NewGame, NewCommand, true, undefined);
                                false ->
                                    case restart_running_after_turn(Maze, NewX, NewY, DeltaX, DeltaY, -1) of
                                        true ->
                                            NewCommand = deltas_to_direction(-DeltaY, -DeltaX),
                                            move(NewGame, NewCommand, true, undefined);
                                        false ->
                                            NewGame
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

is_of_interest(Maze, X, Y) ->
    maze:is_door(Maze, X, Y) orelse
    maze:is_stairs(Maze, X, Y) orelse
    maze:is_item(Maze, X, Y).

%% Stop running when:
%% - the next cell in the current direction is not empty,
%% - the cell is next to a door,
%% - the corridor gets wider (from its narrowest point during the current run).
stop_running(Maze, X, Y, DeltaX, DeltaY, IsEmptyOrt1, IsEmptyOrt2) ->
    {NextX, NextY} = {X + DeltaX, Y + DeltaY},

    {Ort1X, Ort1Y} = {X + DeltaY, Y + DeltaX},
    {Ort2X, Ort2Y} = {X - DeltaY, Y - DeltaX},

    Result = not maze:is_empty(Maze, NextX, NextY) orelse
        is_of_interest(Maze, NextX, NextY) orelse
        is_of_interest(Maze, Ort1X, Ort1Y) orelse
        is_of_interest(Maze, Ort2X, Ort2Y),

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
        not is_of_interest(Maze, Ort1X, Ort1Y) andalso
        not maze:is_empty(Maze, Ort2X, Ort2Y).
