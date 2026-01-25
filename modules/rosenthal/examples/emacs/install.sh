#!/bin/sh
# SPDX-FileCopyrightText: 2026 Hilton Chain <hako@ultrarare.space>
#
# SPDX-License-Identifier: CC0-1.0

grep --recursive --no-filename '^;;guix:' . |
    sed --regexp-extended 's/^;;guix:(.*)/"\1"/g' |
    xargs guix install emacs-pgtk

[[ ! -e ~/.config/emacs/init.el ]] && cp --recursive . ~/.config/emacs
