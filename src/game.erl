-module(game).

-export([
    play/0
]).

-include("maze.hrl").

play() ->
    console:start(),
    
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
    play(Game).

play({game, GameData} = Game) ->
    console:draw_screen(Game),
    Command = console:get_command(),
    console:clear_message(),

    case Command of
        <<"?">> ->
            console:help(),
            play(Game);
        <<"q">> ->
            console:quit();
        Go ->
            case lists:member(Go, [<<"i">>, <<"k">>, <<"j">>, <<"l">>]) of
                true ->
                    Maze = maps:get(maze, GameData),
                    
                    {hero, HeroData} = maps:get(hero, GameData),
                    {X, Y} = maps:get(position, HeroData),
                    Strength = maps:get(strength, HeroData),
    
                    {stats, StatsData} = maps:get(stats, GameData),
    
                    {NewX, NewY} = case Go of
                        <<"i">> ->
                            {X, Y - 1};
                        <<"k">> ->
                            {X, Y + 1};
                        <<"j">> ->
                            {X - 1, Y};
                        <<"l">> ->
                            {X + 1, Y}
                    end,
    
                    NewPosition = case maze:is_empty(Maze, NewX, NewY) of
                        true ->
                            {NewX, NewY};
                        _ ->
                            {X, Y}
                    end,
    
                    NewStrength = case rand:uniform(?ReciprocalStrengthLossOnMove) of
                        1 ->
                            Strength - 1;
                        _ ->
                            Strength
                    end,

                    case Strength of
                        0 ->
                            console:dead();
                        _ ->
                            play({game, GameData#{
                                hero => {hero, HeroData#{
                                    position => NewPosition,
                                    strength => NewStrength
                                }},
                                stats => {stats, StatsData#{
                                    turn => maps:get(turn, StatsData) + 1
                                }}
                            }})
                    end;
                false ->
                    console:hint(),
                    play(Game)
            end
    end.