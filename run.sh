#!/bin/bash
stty -f /dev/tty icanon raw
erl -pa ./src/ -run game play -run init stop -noshell
stty echo echok icanon -raw
