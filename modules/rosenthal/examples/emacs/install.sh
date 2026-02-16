#!/bin/sh
dir="$(dirname "$(realpath "$0")")"
[[ ! -e ~/.config/emacs/init.el ]] && cp --recursive "$dir/." ~/.config/emacs
guix package --install --manifest="$dir/manifest.scm" --dry-run
