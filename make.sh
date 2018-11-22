#!/bin/bash

if [ ! -d ebin ]; then
    mkdir ebin
fi

erlc -o ebin src/*.erl
