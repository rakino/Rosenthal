#!/bin/sh

grep --recursive --no-filename '^;;guix:' . |
    sed --regexp-extended 's/^;;guix:(.*)/"\1"/g' |
    xargs guix install emacs-pgtk

[[ ! -e ~/.config/emacs/init.el ]] && cp --recursive . ~/.config/emacs
