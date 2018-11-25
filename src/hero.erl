-module(hero).

-export([
    initialize_hero/1
]).

-include("board.hrl").
-include("hero.hrl").

initialize_hero(Maze) ->
    X = rand:uniform(?BoardWidth),
    Y = rand:uniform(?BoardHeight),
    
    case maze:is_empty(Maze, X, Y) of
         true ->
             {hero, #{
                 position => {X, Y},
                 running => false,
                 strength => ?InitialStrength
             }};
         false ->
             initialize_hero(Maze)
    end.