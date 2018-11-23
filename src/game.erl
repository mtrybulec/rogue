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
    console:draw_info(Game),
    Command = console:get_command(),
    console:clear_message(),

    case Command of
        command_debug ->
            console:debug(Game),
            play(Game);
        command_restart ->
            play();
        command_help ->
            console:help(Game),
            play(Game);
        command_quit ->
            console:quit();
        _ ->
            case lists:member(Command, [command_move_up, command_move_down, command_move_left, command_move_right]) of
                true ->
                    Maze = maps:get(maze, GameData),
                    
                    {hero, HeroData} = maps:get(hero, GameData),
                    {X, Y} = maps:get(position, HeroData),
                    Strength = maps:get(strength, HeroData),
    
                    {stats, StatsData} = maps:get(stats, GameData),
    
                    {NewX, NewY} = case Command of
                        command_move_up ->
                            {X, Y - 1};
                        command_move_down ->
                            {X, Y + 1};
                        command_move_left ->
                            {X - 1, Y};
                        command_move_right ->
                            {X + 1, Y}
                    end,
    
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

                    NewGame = {game, GameData#{
                        hero => {hero, HeroData#{
                            position => NewPosition,
                            strength => NewStrength
                        }},
                        stats => {stats, StatsData#{
                            turn => maps:get(turn, StatsData) + 1
                        }}
                    }},
                    
                    case NewStrength of
                        0 ->
                            console:dead(NewGame);
                        _ ->
                            console:move(Maze, {X, Y}, NewPosition),
                            play(NewGame)
                    end;
                false ->
                    console:hint(),
                    play(Game)
            end
    end.