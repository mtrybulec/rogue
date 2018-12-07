-module(game).

-export([
    play/0
]).

-define(LevelCount, 3).
-define(ReciprocalStrengthLossOnMove, 10).
-define(StrengthLossOnInvalidCommand, 1).
-define(StrengthLossOnAttack, 1).
-define(HitStrength, 6).

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
                        _ ->
                            GameNextTurn = increment_turn(Game),
                            GamePerformedCommand = perform_command(GameNextTurn, Command, Running, get_vicinity(GameNextTurn, Command)),
                            play(GamePerformedCommand)
                    end
            end
    end.

increment_turn(Game) ->
    Stats = maps:get(stats, Game),
    NewStats = Stats#{
        turn => maps:get(turn, Stats) + 1
    },
    Game#{stats => NewStats}.

get_vicinity(Game, Command) ->
    Maze = maps:get(maze, Game),
    Hero = maps:get(hero, Game),
    
    {X, Y} = maps:get(position, Hero),
    {DeltaX, DeltaY} = direction_to_deltas(Command),
    
    IsEmptyOrt1 = maze:is_empty(Maze, X + DeltaY, Y + DeltaX),
    IsEmptyOrt2 = maze:is_empty(Maze, X - DeltaY, Y - DeltaX),
    
    {IsEmptyOrt1, IsEmptyOrt2}.

perform_command(Game, Command, Running, Vicinity) ->
    {GameHeroMoved, RunningHeroMoved} = case Command of
        collect_item ->
            {collect_item(Game), false};
        take_stairs ->
            {take_stairs(Game), false};
        _ ->
            move(Game, Command, Running)
    end,
    
    HeroMoved = maps:get(hero, GameHeroMoved),
    StrengthHeroMoved = maps:get(strength, HeroMoved),
    
    GameMonstersMoved = case StrengthHeroMoved =< 0 of
        true ->
            GameHeroMoved;
        false ->
            MazeHeroMoved = maps:get(maze, GameHeroMoved),
            PositionHeroMoved = maps:get(position, HeroMoved),
            
            {MazeMonstersMoved, StrengthMonstersMoved} =
                handle_monsters_move(MazeHeroMoved, PositionHeroMoved, StrengthHeroMoved),
            
            GameHeroMoved#{
                maze => MazeMonstersMoved,
                hero => HeroMoved#{
                    strength => StrengthMonstersMoved
                }
            }
    end,
        
    case Command of
        collect_item ->
            GameMonstersMoved;
        take_stairs ->
            GameMonstersMoved;
        _ ->
            continue_running(GameMonstersMoved, Command, RunningHeroMoved, Vicinity)
    end.

collect_item(Game) ->
    Hero = maps:get(hero, Game),
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
                }
            };
        false ->
            NewStrength = Strength - ?StrengthLossOnInvalidCommand,
            
            Game#{
                hero => Hero#{strength => NewStrength}
            }
    end.

take_stairs(Game) ->
    Hero = maps:get(hero, Game),
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
                }
            },
            
            console:clear_screen(),
            console:draw_board(NewGame),
            
            NewGame;
        false ->
            NewStrength = Strength - ?StrengthLossOnInvalidCommand,
            
            Game#{
                hero => Hero#{strength => NewStrength}
            }
    end.

move(Game, Command, Running) ->
    Hero = maps:get(hero, Game),
    Maze = maps:get(maze, Game),
    {X, Y} = maps:get(position, Hero),
    {DeltaX, DeltaY} = direction_to_deltas(Command),
    {NewX, NewY} = {X + DeltaX, Y + DeltaY},
    
    NewPosition = case maze:is_empty(Maze, NewX, NewY) andalso not maze:is_monster(Maze, NewX, NewY) of
        true ->
            {NewX, NewY};
        _ ->
            {X, Y}
    end,
    
    Strength = maps:get(strength, Hero),

    {MazeHeroMoved, StrengthHeroMoved, RunningHeroMoved} = handle_hero_move(Maze, {NewX, NewY}, Strength, Running),
    
    console:move_hero(MazeHeroMoved, {X, Y}, NewPosition),
    
    HeroHeroMoved = Hero#{
        position => NewPosition,
        strength => StrengthHeroMoved
    },
    GameHeroMoved = Game#{
        maze => MazeHeroMoved,
        hero => HeroHeroMoved
    },
    
    {GameHeroMoved, RunningHeroMoved}.

handle_hero_move(Maze, {X, Y}, Strength, Running) ->
    case maze:is_empty(Maze, X, Y) of
        false ->
            %% Don't hit the wall - you'll hurt yourself!
            {Maze, Strength - ?StrengthLossOnInvalidCommand, false};
        true ->
            case maze:is_monster(Maze, X, Y) of
                true ->
                    {Monster, MazeNoMonster} = maze:remove_monster(Maze, X, Y),
                    {MonsterType, MonsterStrength} = Monster,
                    MonsterStrengthAfterAttack = MonsterStrength - rand:uniform(?HitStrength) - rand:uniform(?HitStrength),
            
                    MazeAfterAttack = case MonsterStrengthAfterAttack =< 0 of
                        true ->
                            console:update(MazeNoMonster, X, Y),
                            MazeNoMonster;
                        false ->
                            [{monster, {X, Y}, {MonsterType, MonsterStrengthAfterAttack}}] ++ MazeNoMonster
                    end,
            
                    {MazeAfterAttack, Strength - ?StrengthLossOnAttack, false};
                false ->
                    %% Walking around saps energy; running - even more so...
                    StrengthAfterMove = case rand:uniform(?ReciprocalStrengthLossOnMove div (util:boolean_to_integer(Running) + 1)) of
                        1 ->
                            Strength - 1;
                        _ ->
                            Strength
                    end,

                    {Maze, StrengthAfterMove, Running}
            end
    end.

handle_monsters_move(Maze, {X, Y}, Strength) ->
    handle_monsters_move(Maze, {X, Y}, Strength, []).

handle_monsters_move([{monster, {MX, MY}, {MonsterType, MonsterStrength}} = Monster | T], {HX, HY}, Strength, NewMaze) ->
    {NewStrength, NewMonster} = case (abs(MX - HX) == 1 andalso abs(MY - HY) == 0) orelse
                                     (abs(MX - HX) == 0 andalso abs(MY - HY) == 1) of
        true ->
            {max(0, Strength - rand:uniform(?HitStrength)), Monster};
        false ->
            DeltaX = util:sign(HX - MX),
            DeltaY = util:sign(HY - MY),
            
            NewMX = MX + DeltaX,
            NewMY = MY + DeltaY,
            
            case maze:is_empty(T ++ NewMaze, NewMX, NewMY) andalso (NewMX =/= HX orelse NewMY =/= HY) andalso not maze:is_monster(T ++ NewMaze, NewMX, NewMY) of
                true ->
                    console:move_monster(T ++ NewMaze, {MX, MY}, {NewMX, NewMY}, MonsterType),
                    {Strength, {monster, {NewMX, NewMY}, {MonsterType, MonsterStrength}}};
                false ->
                    {Strength, Monster}
            end
    end,
    handle_monsters_move(T, {HX, HY}, NewStrength, [NewMonster] ++ NewMaze);
handle_monsters_move([H | T], {X, Y}, Strength, NewMaze) ->
    handle_monsters_move(T, {X, Y}, Strength, [H] ++ NewMaze);
handle_monsters_move([], {_X, _Y}, Strength, NewMaze) ->
    {NewMaze, Strength}.

continue_running(Game, Command, Running, {IsEmptyOrt1, IsEmptyOrt2}) ->
    Hero = maps:get(hero, Game),
    Strength = maps:get(strength, Hero),
    
    case Strength =< 0 orelse not Running of
        true ->
            Game;
        false ->
            Maze = maps:get(maze, Game),
            {X, Y} = maps:get(position, Hero),
            {DeltaX, DeltaY} = direction_to_deltas(Command),
            
            {StopRunning, NewIsEmptyOrt1, NewIsEmptyOrt2} =
                stop_running(Maze, X, Y, DeltaX, DeltaY, IsEmptyOrt1, IsEmptyOrt2),
            
            case StopRunning of
                false ->
                    perform_command(Game, Command, true, {NewIsEmptyOrt1, NewIsEmptyOrt2});
                true ->
                    NewCommand = case restart_running_after_turn(Maze, X, Y, DeltaX, DeltaY, 1) of
                        true ->
                            deltas_to_direction(DeltaY, DeltaX);
                        false ->
                            case restart_running_after_turn(Maze, X, Y, DeltaX, DeltaY, -1) of
                                true ->
                                    deltas_to_direction(-DeltaY, -DeltaX);
                                false ->
                                    undefined
                            end
                    end,
                    
                    case NewCommand of
                        undefined ->
                            Game;
                        _ ->
                            perform_command(Game, NewCommand, true, get_vicinity(Game, NewCommand))
                    end
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
            {1, 0};
        _ ->
            {0, 0}
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
    maze:is_item(Maze, X, Y) orelse
    maze:is_monster(Maze, X, Y).

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
