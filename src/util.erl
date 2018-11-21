-module(util).

-export([
   is_shell/0
]).

is_shell() ->
    CliArguments = init:get_arguments(),
    not lists:member({noshell, []}, CliArguments).