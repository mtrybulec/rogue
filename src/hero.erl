-module(hero).

-export([
    initialize_hero/1,
    initialize_hero_position/1
]).

-define(InitialStrength, 100).

initialize_hero_position(Maze) ->
    {X, Y} = board:generate_point(),
    
    case maze:is_empty(Maze, X, Y) of
        true ->
            {X, Y};
        false ->
            initialize_hero_position(Maze)
    end.

initialize_hero(Maze) -> #{
    items => [],
    level => 1,
    position => initialize_hero_position(Maze),
    strength => ?InitialStrength
}.