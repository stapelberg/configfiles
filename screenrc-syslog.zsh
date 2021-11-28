#!/bin/zsh

print -rn -- $'\e'"]2;midna syslog"$'\a'
journalctl -t kernel -f -n 500 | grep -v audit
