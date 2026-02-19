#!/bin/sh
dir="$(dirname "$(realpath "$0")")"
if [[ ! -f ~/.config/emacs/init.el ]]
then
    mkdir -p ~/.config/emacs
    cp "$dir/init"* ~/.config/emacs
fi
cat "$dir/packages.txt" | xargs guix package --install
