;;; -*- lexical-binding: t -*-
;;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(setopt custom-file (locate-user-emacs-file "custom.el"))
(if (not (file-exists-p custom-file))
    (make-empty-file custom-file)
  (load custom-file))

;; Tweak garbage collection strategy.
;;guix:emacs-gcmh
(use-package gcmh
  :config
  (gcmh-mode 1))

;; Set default storage locations for various packages.
;;guix:emacs-no-littering
(use-package no-littering
  :config
  (no-littering-theme-backups))

(use-package emacs
  :custom
  ;; Workaround to use fish as login shell.
  (shell-file-name "/bin/sh"))

(load-file (locate-user-emacs-file "fonts.el"))
(load-file (locate-user-emacs-file "interface.el"))
(load-file (locate-user-emacs-file "editing.el"))
(load-file (locate-user-emacs-file "miscellaneous.el"))


;;;
;;; Set up initial screen.
;;;

(progn
  (setopt initial-scratch-message
          ";;; Type your Guile program here and evaluate it.\n\n")
  (scheme-mode)
  (geiser-repl-import-module "(gnu)")
  (geiser-repl-import-module "(nonguix)")
  (geiser-repl-import-module "(rosenthal)")
  (delete-window)
  (display-splash-screen))
