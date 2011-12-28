#!/bin/bash
# vim:ts=4:sw=4:expandtab
#
# © 2011 Michael Stapelberg, Public Domain

cfgfiles=$(dirname $(readlink ~/.zshrc))
echo "NOTE: updating git in $cfgfiles"
zsh -c "(cd $cfgfiles && (git stash && git pull; git stash apply)) &" >$cfgfiles/last-update.log 2>&1

