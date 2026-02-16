;;; -*- lexical-binding: t -*-

(setopt custom-file (locate-user-emacs-file "custom.el"))
(if (not (file-exists-p custom-file))
    (make-empty-file custom-file)
  (load custom-file))

;; Tweak garbage collection strategy.
(use-package gcmh
  :config
  (gcmh-mode 1))

;; Set default storage locations for various packages.
(use-package no-littering
  :config
  (no-littering-theme-backups))

(use-package emacs
  :custom
  ;; Workaround to use fish as login shell.
  (shell-file-name "/bin/sh"))

(load-file (locate-user-emacs-file "init-fonts.el"))
(load-file (locate-user-emacs-file "init-interface.el"))
(load-file (locate-user-emacs-file "init-editing.el"))
(load-file (locate-user-emacs-file "init-miscellaneous.el"))


;;;
;;; Set up initial screen.
;;;

(progn
  (setopt initial-scratch-message
          "\
;;; Type your Guile program here and evaluate it.
;;; `M-x cua-mode' to use Ctrl-C/X/Z for copy, cut, paste.
\n")
  (scheme-mode)
  (geiser-repl-import-module "(gnu)")
  (geiser-repl-import-module "(nonguix)")
  (geiser-repl-import-module "(rosenthal)")
  (delete-window)
  (display-splash-screen))
