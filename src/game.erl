-module(game).

-export([
    play/0
]).

play() ->
    Maze = maze:generate_maze(),
    Hero = hero:initialize_hero(Maze),
    console:draw_screen(Maze, Hero).