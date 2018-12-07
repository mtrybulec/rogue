-module(board).

-export([
    generate_point/0
]).

-include("board.hrl").

generate_point() ->
    X = rand:uniform(?BoardWidth),
    Y = rand:uniform(?BoardHeight),
    {X, Y}.
