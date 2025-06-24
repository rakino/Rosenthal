;;; -*- lexical-binding: t -*-
;;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

;; TODO:
;; 1. Configure geiser.  Take a look at guile-studio.
;; 2. Make use of ‘guix:’ comments.

;;;
;;; Init
;;;

(setopt custom-file (locate-user-emacs-file "custom.el"))
(if (not (file-exists-p custom-file))
    (make-empty-file custom-file)
  (load custom-file))

(load-file (locate-user-emacs-file "fonts.el"))

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
  (shell-file-name "/bin/sh")          ;Workaround to use fish as login shell.
  (word-wrap-by-category t))           ;CJK support.


;;;
;;; Interface
;;;

(use-package emacs
  :custom
  (blink-cursor-mode nil)
  (browse-url-firefox-program "librewolf")
  (enable-recursive-minibuffers t)
  ;; Scrolling enhancement.
  (pixel-scroll-precision-mode t)
  ;; Exclude unavailable completions.
  (read-extended-command-predicate 'command-completion-default-include-p)
  ;; Case-insensitive completion.
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  :bind
  ([remap list-buffers] . switch-to-buffer)
  :hook
  ;; Indicatior for recursive minibuffers.
  (after-init . minibuffer-depth-indicate-mode)
  ;; Save minibuffer history.
  (after-init . savehist-mode)
  ;; Indicator for `fill-column'.
  (prog-mode . display-fill-column-indicator-mode))

;; Theming
(use-package emacs
  :custom
  (fringe-mode 0)
  (modus-themes-italic-constructs t)
  (scroll-bar-mode nil)
  (tool-bar-mode nil)
  (tooltip-mode nil)
  :config
  (load-theme 'modus-operandi-tinted :no-confirm)
  :hook
  (after-init . menu-bar-mode))

(use-package completion-preview
  :custom
  (global-completion-preview-mode t))

;;guix:emacs-doom-modeline
(use-package doom-modeline
  :custom
  (doom-modeline-icon nil)
  (doom-modeline-height 18)
  :hook
  (after-init . doom-modeline-mode))

;;guix:emacs-helpful
(use-package helpful
  :bind
  (([remap describe-function] . helpful-callable)
   ([remap describe-variable] . helpful-variable)
   ([remap describe-key]      . helpful-key)
   ([remap describe-command]  . helpful-command)
   ([remap describe-symbol]   . helpful-symbol)
   ("C-c C-d" . helpful-at-point)))

;;guix:emacs-hl-todo
(use-package hl-todo
  :custom
  (hl-todo-highlight-punctuation ":")
  :hook
  (prog-mode . hl-todo-mode))

(use-package isearch
  :custom
  (isearch-lazy-count t)
  (isearch-wrap-pause 'no-ding)
  (regexp-search-ring-max 200)
  (search-ring-max 200))

;;guix:emacs-mwim
(use-package mwim
  :bind
  (([remap move-beginning-of-line] . mwim-beginning)
   ([remap move-end-of-line] . mwim-end)))

;;guix:emacs-orderless
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-matching-styles '(orderless-literal orderless-flex orderless-regexp)))

(use-package paren
  :custom
  (show-paren-context-when-offscreen 'overlay)
  :hook
  (after-init . show-paren-mode))

;;guix:emacs-rainbow-delimiters
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

;;guix:emacs-vertico
(use-package vertico
  :hook
  (after-init . vertico-mode))

(use-package vertico-directory
  :after (vertico)
  :bind
  (:map vertico-map
        ("RET" . vertico-directory-enter)
        ("DEL" . vertico-directory-delete-char)
        ("M-DEL" . vertico-directory-delete-word))
  :hook
  (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package which-key
  :config
  (which-key-setup-side-window-right-bottom)
  :hook
  (after-init . which-key-mode))


;;;
;;; Editing
;;;

(use-package emacs
  :custom
  ;; Disable tab indentation.
  (indent-tabs-mode nil)
  :config
  ;; Avoid re-indenting current line after entering ‘RET’.
  (setopt electric-indent-inhibit t)
  :hook
  (before-save . delete-trailing-whitespace)
  ;; Automatic parenthesis pairing.
  (after-init . electric-pair-mode))

;;guix:emacs-macrostep
(use-package macrostep
  :bind
  ("C-c e" . macrostep-expand))

;;guix:emacs-puni
(use-package puni
  :hook
  ((eval-expression-minibuffer-setup nxml-mode prog-mode sgml-mode tex-mode)
   . puni-mode))


;;;
;;; Miscellaneous
;;;

;;guix:emacs-daemons
(use-package daemons
  :custom
  (daemons-list-fill-frame t))

(use-package dired
  :custom
  (dired-listing-switches
   "-lv --all --group-directories-first --human-readable")
  :bind
  ([remap list-directory] . dired))

(use-package dired-aux
  :custom
  (dired-compress-directory-default-suffix ".tar.zst")
  (dired-create-destination-dirs 'ask)
  (dired-create-destination-dirs-on-trailing-dirsep t)
  (dired-vc-rename-file t))

;;guix:emacs-envrc
(use-package envrc
  :hook
  (after-init . envrc-global-mode))

;;guix:emacs-magit
(use-package magit
  :custom
  (git-commit-cd-to-toplevel t)
  :config
  (add-to-list 'project-switch-commands '(magit-project-status "Magit") t)
  :bind
  (:map project-prefix-map
        ("m" . magit-project-status)))

;;guix:emacs-forge
(use-package forge
  :after (magit))
