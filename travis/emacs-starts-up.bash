#!/bin/bash

set -eux

ln -sf $HOME/configfiles/config/emacs .emacs.d
# Explicitly load init.el because --batch implies -q.
# Run magit-status, which triggers more delayed package
# installation.
emacs \
    --debug-init \
    --batch \
    --eval='(load-file "$PWD/.emacs.d/init.el")' \
    --eval='(magit-status)' \
    --kill
