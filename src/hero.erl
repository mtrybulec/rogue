-module(hero).

-export([
    initialize_hero/1
]).

-include("maze.hrl").

initialize_hero(Maze) ->
    PosX = rand:uniform(?ScreenWidth),
    PosY = rand:uniform(?ScreenHeight),
    
    case maze:is_empty(Maze, PosX, PosY) of
         true ->
             {hero, [PosX, PosY]};
         false ->
            initialize_hero(Maze)
    end.