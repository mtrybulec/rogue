#!/bin/bash

# Change terminal mode to send characters to standard input as soon as type -
# don't wait for the Enter at the end of the line.
# https://stackoverflow.com/questions/42750491/read-a-character-input-from-erlang-without-requiring-the-return-key-pressed-from
# Updated so that io:format("~n") does a CR-NL.

save_tty_state=$(stty -g)
stty raw opost onlret
erl -pa ./src/ -run game play -run init stop -noshell
stty "$save_tty_state"
