#!/bin/sh
set -e

TOP_DIR="$(dirname "$(realpath "$0")")"

# Install packages.
guix repl -- "$TOP_DIR/packages"

# Install configuration file.
if [ ! -f ~/.config/emacs/init.el ]
then
    install -D -mode=0644 --target-directory="$HOME/.config/emacs" "$TOP_DIR/init.el"
fi
