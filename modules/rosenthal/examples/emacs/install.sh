#!/bin/sh
TOP_DIR="$(dirname "$(realpath "$0")")"

# Install packages.
echo "
(use-modules (guix packages)
             (guix scripts install))

(apply guix-install
       (map package-name
            (load \"$TOP_DIR/packages\")))" |
    guix repl --type=machine

# Install configuration file.
if [ ! -f ~/.config/emacs/init.el ]
then
    install -D -mode=0644 --target-directory="$HOME/.config/emacs" "$TOP_DIR/init.el"
fi
