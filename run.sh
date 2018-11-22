#!/bin/bash

# Change terminal mode to send characters to standard input as soon as type -
# don't wait for the Enter at the end of the line.
# https://stackoverflow.com/questions/42750491/read-a-character-input-from-erlang-without-requiring-the-return-key-pressed-from
# Updated so that io:format("~n") does a CR-NL.

set -e
./make.sh

save_tty_state=$(stty -g)
stty raw opost onlret
set +e
erl -pa ebin -run game play -run init stop -noshell
stty "$save_tty_state"

