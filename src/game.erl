-module(game).

-export([
    play/0
]).

-define(LevelCount, 3).
-define(ReciprocalStrengthLossOnMove, 10).
-define(StrengthLossOnInvalidCommand, 1).

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
    Hero = maps:get(hero, Game),
    Strength = maps:get(strength, Hero),
    
    case Strength =< 0 of
        true ->
            console:dead(Game),
            rip;
        false ->
            Items = maps:get(items, Hero),
    
            case lists:any(fun(Item) -> Item == treasure end, Items) of
                true ->
                    console:done(),
                    done;
                false ->
                    console:draw_info(Game),
                    {Command, Running} = console:get_command(Game),
            
                    case Command of
                        restart_game ->
                            play();
                        quit_game ->
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
    Hero = maps:get(hero, Game),
    Stats = maps:get(stats, Game),
    NewStats = Stats#{
        turn => maps:get(turn, Stats) + 1
    },
    Maze = maps:get(maze, Game),
    {X, Y} = maps:get(position, Hero),
    Strength = maps:get(strength, Hero),
    
    case maze:is_item(Maze, X, Y) of
        true ->
            Items = maps:get(items, Hero),
            {Item, NewMaze} = maze:remove_item(Maze, X, Y),
    
            NewStrength = case rand:uniform(?ReciprocalStrengthLossOnMove) of
                1 ->
                    Strength - 1;
                _ ->
                    Strength
            end,
    
            Game#{
                maze => NewMaze,
                hero => Hero#{
                    items => [Item] ++ Items,
                    strength => NewStrength
                },
                stats => NewStats
            };
        false ->
            NewStrength = Strength - ?StrengthLossOnInvalidCommand,
            
            Game#{
                hero => Hero#{strength => NewStrength},
                stats => NewStats
            }
    end.

take_stairs(Game) ->
    Hero = maps:get(hero, Game),
    Stats = maps:get(stats, Game),
    NewStats = Stats#{
        turn => maps:get(turn, Stats) + 1
    },
    Maze = maps:get(maze, Game),
    {X, Y} = maps:get(position, Hero),
    Strength = maps:get(strength, Hero),
    
    case maze:is_stairs(Maze, X, Y) of
        true ->
            Level = maps:get(level, Hero),
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
                hero => Hero#{
                    level => NewLevel,
                    position => Position,
                    strength => NewStrength
                },
                stats => NewStats
            },
            
            console:clear_screen(),
            console:draw_board(NewGame),
            
            NewGame;
        false ->
            NewStrength = Strength - ?StrengthLossOnInvalidCommand,
            
            Game#{
                hero => Hero#{strength => NewStrength},
                stats => NewStats
            }
    end.

move(Game, Command, Running, undefined) ->
    Maze = maps:get(maze, Game),
    Hero = maps:get(hero, Game),
   
    {X, Y} = maps:get(position, Hero),
    {DeltaX, DeltaY} = direction_to_deltas(Command),
   
    IsEmptyOrt1 = maze:is_empty(Maze, X + DeltaY, Y + DeltaX),
    IsEmptyOrt2 = maze:is_empty(Maze, X - DeltaY, Y - DeltaX),
    
    move(Game, Command, Running, {IsEmptyOrt1, IsEmptyOrt2});
move(Game, Command, Running, {IsEmptyOrt1, IsEmptyOrt2}) ->
    Hero = maps:get(hero, Game),
    Stats = maps:get(stats, Game),
    NewStats = Stats#{
        turn => maps:get(turn, Stats) + 1
    },
    Maze = maps:get(maze, Game),
    {X, Y} = maps:get(position, Hero),
    {DeltaX, DeltaY} = direction_to_deltas(Command),
    {NewX, NewY} = {X + DeltaX, Y + DeltaY},
    
    NewPosition = case maze:is_empty(Maze, NewX, NewY) of
        true ->
            {NewX, NewY};
        _ ->
            {X, Y}
    end,
    
    Strength = maps:get(strength, Hero),

    NewStrength = case maze:is_empty(Maze, NewX, NewY) of
        false ->
            %% Don't hit the wall - you'll hurt yourself!
            Strength - ?StrengthLossOnInvalidCommand;
        true ->
            %% Walking around saps energy; running even more so...
            case rand:uniform(?ReciprocalStrengthLossOnMove div (util:boolean_to_integer(Running) + 1)) of
                1 ->
                    Strength - 1;
                _ ->
                    Strength
            end
    end,
    
    Stats = maps:get(stats, Game),
    
    NewGame = Game#{
        hero => Hero#{
            position => NewPosition,
            strength => NewStrength
        },
        stats => NewStats
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
        move_north ->
            {0, -1};
        move_south ->
            {0, 1};
        move_west ->
            {-1, 0};
        move_east ->
            {1, 0}
    end.

deltas_to_direction(DeltaX, DeltaY) ->
    case {DeltaX, DeltaY} of
        {0, -1} ->
            move_north;
        {0, 1} ->
            move_south;
        {-1, 0} ->
            move_west;
        {1, 0} ->
            move_east
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
