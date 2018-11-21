-module(hero).

-export([
    initialize_hero/1
]).

-include("maze.hrl").

initialize_hero(Maze) ->
    X = rand:uniform(?ScreenWidth),
    Y = rand:uniform(?ScreenHeight),
    
    case maze:is_empty(Maze, X, Y) of
         true ->
             {hero, #{
                 position => {X, Y}
             }};
         false ->
             initialize_hero(Maze)
    end.