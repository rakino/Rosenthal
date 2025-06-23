;;; -*- lexical-binding: t -*-
;;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

;;;
;;; Init
;;;

(use-package emacs
  :custom
  (blink-cursor-mode nil)
  (browse-url-firefox-program "librewolf")
  (completion-ignore-case t)
  (enable-recursive-minibuffers t)
  (fringe-mode 0)
  (global-completion-preview-mode t)
  (indent-tabs-mode nil)
  (pixel-scroll-precision-mode t)
  (read-buffer-completion-ignore-case t)
  (read-extended-command-predicate 'command-completion-default-include-p)
  (read-file-name-completion-ignore-case t)
  (scroll-bar-mode nil)
  (shell-file-name "/bin/sh")
  (tool-bar-mode nil)
  (tooltip-mode nil)
  (word-wrap-by-category t)
  (modus-themes-italic-constructs t)
  :init
  (setopt custom-file (locate-user-emacs-file "custom.el"))
  (if (not (file-exists-p custom-file))
      (make-empty-file custom-file)
    (load custom-file))
  (gcmh-mode 1)
  (no-littering-theme-backups)
  (load-file (locate-user-emacs-file "fonts.el"))
  :config
  (setopt electric-indent-inhibit t)
  :hook
  ((after-init . electric-pair-mode)
   (after-init . menu-bar-mode)
   (after-init . minibuffer-depth-indicate-mode)
   (after-init . savehist-mode)
   (before-save . delete-trailing-whitespace)
   (prog-mode . display-fill-column-indicator-mode))
  :bind (([remap list-buffers] . switch-to-buffer)))


;;;
;;; Interface
;;;

(use-package emacs
  :custom
  (modus-themes-italic-constructs t)
  :config
  (load-theme 'modus-operandi-tinted :no-confirm))

(use-package doom-modeline
  :custom
  (doom-modeline-icon nil)
  (doom-modeline-height 18)
  :hook
  (after-init . doom-modeline-mode))

(use-package helpful
  :bind
  (([remap describe-function] . helpful-callable)
   ([remap describe-variable] . helpful-variable)
   ([remap describe-key]      . helpful-key)
   ([remap describe-command]  . helpful-command)
   ([remap describe-symbol]   . helpful-symbol)
   ("C-c C-d" . helpful-at-point)))

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

(use-package mwim
  :bind
  (([remap move-beginning-of-line] . mwim-beginning)
   ([remap move-end-of-line] . mwim-end)))

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

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

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

(use-package macrostep
  :bind
  ("C-c e" . macrostep-expand))

(use-package puni
  :hook
  ((eval-expression-minibuffer-setup nxml-mode prog-mode sgml-mode tex-mode) . puni-mode))


;;;
;;; Miscellaneous
;;;

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

(use-package envrc
  :hook
  (after-init . envrc-global-mode))

(use-package magit
  :custom
  (git-commit-cd-to-toplevel t)
  :config
  (add-to-list 'project-switch-commands '(magit-project-status "Magit") t)
  :bind
  (:map project-prefix-map
        ("m" . magit-project-status)))

(use-package forge
  :after (magit))
