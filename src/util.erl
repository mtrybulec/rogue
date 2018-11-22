-module(util).

-export([
   is_shell/0
]).

is_shell() ->
    case init:get_argument(noshell) of
        {ok, _} ->
            false;
        error ->
            true
    end.