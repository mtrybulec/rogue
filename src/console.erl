%% For tty control escape sequences, see
%% http://ascii-table.com/ansi-escape-sequences.phphttp://ascii-table.com/ansi-escape-sequences.php

-module(console).

-export([
    clear_screen/0,
    goto_xy/2
]).

clear_screen() ->
    io:format("\033[2J").

goto_xy(X, Y) ->
    io:format("\033[~w;~wH", [Y, X]).