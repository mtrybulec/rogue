-module(game).

-export([
    play/0
]).

-include("maze.hrl").

play() ->
    Maze = maze:generate_maze(),
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
    console:draw_screen(Game),
    play(Game).

play({game, GameData} = Game) ->
    Hero = maps:get(hero, GameData),

    console:draw_info(Game),
    {NewHero, Command} = console:get_command(Hero),
    NewGame = {game, GameData#{hero => NewHero}},
    console:clear_message(),

    case Command of
        command_debug ->
            console:debug(NewGame),
            play(NewGame);
        command_restart ->
            play();
        command_help ->
            console:help(NewGame),
            play(NewGame);
        command_quit ->
            console:quit();
        _ ->
            case lists:member(Command, [command_move_up, command_move_down, command_move_left, command_move_right]) of
                true ->
                    GameAfterMove = move(NewGame, Command),
                    case GameAfterMove of
                        rip ->
                            rip;
                        _ ->
                            play(GameAfterMove)
                    end;
                false ->
                    console:hint(),
                    play(NewGame)
            end
    end.

move({game, GameData} = _Game, Command) ->
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
            console:dead(NewGame);
        _ ->
            console:move(Maze, {X, Y}, NewPosition),
            case maps:get(running, HeroData) of
                {true, _Direction} ->
                    {NextX, NextY} = {NewX + DeltaX, NewY + DeltaY},
                    case maze:is_empty(Maze, NextX, NextY)andalso
                        not maze:is_door(Maze, NextX, NextY) of
                        true ->
                            move(NewGame, Command);
                        false ->
                            StopRunningHeroData = NewHeroData#{running => false},
                            {game, GameData#{hero => {hero, StopRunningHeroData}}}
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