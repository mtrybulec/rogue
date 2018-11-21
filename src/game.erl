-module(game).

-export([
    play/0
]).

-include("maze.hrl").

play() ->
    console:start(),
    
    Maze = maze:generate_maze(),
    Hero = hero:initialize_hero(Maze),
    
    console:clear_screen(),
    play(Maze, Hero).

play(Maze, Hero) ->
    console:draw_screen(Maze, Hero),
    Command = console:get_command(),

    case Command of
        <<"q">> ->
            console:quit();
        Go ->
            case lists:member(Go, [<<"i">>, <<"k">>, <<"j">>, <<"l">>]) of
                true ->
                    {hero, Data} = Hero,
                    {X, Y} = maps:get(position, Data),
                    Strength = maps:get(strength, Data),

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

                    play(Maze, {hero, Data#{
                        position => NewPosition,
                        strength => NewStrength}});
                false ->
                    play(Maze, Hero)
            end
    end.