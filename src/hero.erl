-module(hero).

-export([
    initialize_hero/1,
    initialize_hero_position/1
]).

-define(InitialStrength, 100).

initialize_hero_position(Maze) ->
    maze:generate_empty_point(Maze).

initialize_hero(Maze) -> #{
    items => [],
    level => 1,
    position => initialize_hero_position(Maze),
    strength => ?InitialStrength
}.