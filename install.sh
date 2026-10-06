#!/usr/bin/env sh

ln -sf "$(pwd)/init.el" "$XDG_CONFIG_HOME/emacs/init.el"
ln -sf "$(pwd)/early-init.el" "$XDG_CONFIG_HOME/emacs/early-init.el"
