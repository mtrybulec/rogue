-module(util).

-export([
    is_shell/0,
    sign/1
]).

is_shell() ->
    case init:get_argument(noshell) of
        {ok, _} ->
            false;
        error ->
            true
    end.

sign(X) when X < 0 ->
    -1;
sign(X) when X > 0 ->
    1;
sign(_) ->
    0.