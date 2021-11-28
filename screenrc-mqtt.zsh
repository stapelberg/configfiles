#!/bin/zsh

print -rn -- $'\e'"]2;all mqtt"$'\a'
mosquitto_sub --id 'midna_all' --verbose -h dr -t "#"
