#!/bin/bash

set -eux

wd=$PWD
ln -sf $HOME/configfiles/config/emacs .emacs.d
git clone /root/configfiles configfiles.clone
cd configfiles.clone
# Explicitly load init.el because --batch implies -q.
# Run magit-status, which triggers more delayed package
# installation.
emacs \
    --debug-init \
    --batch \
    --eval="(load-file \"$wd/.emacs.d/init.el\")" \
    --eval="(magit-status)" \
    --kill
