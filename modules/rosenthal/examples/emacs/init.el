;;; -*- lexical-binding: t -*-
;;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(setopt custom-file (locate-user-emacs-file "custom.el"))
(if (not (file-exists-p custom-file))
    (make-empty-file custom-file)
  (load custom-file))

(let ((font-config "$$fonts.el$$"))
  (when (file-exists-p font-config)
    (load-file font-config)))

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
  (shell-file-name "/bin/sh")
  ;; CJK support.
  (word-wrap-by-category t))


;;;
;;; Interface
;;;

(use-package emacs
  :custom
  (blink-cursor-mode nil)
  (browse-url-firefox-program "librewolf")
  (enable-recursive-minibuffers t)
  (inhibit-splash-screen t)
  (uniquify-buffer-name-style 'forward)
  ;; Exclude unavailable completions.
  (read-extended-command-predicate 'command-completion-default-include-p)
  ;; Case-insensitive completion.
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  :bind
  ([remap list-buffers] . switch-to-buffer)
  :hook
  (prog-mode . display-line-numbers-mode)
  ;; Scrolling enhancement.
  (after-init . pixel-scroll-precision-mode)
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

;;guix:emacs-corfu
(use-package corfu
  :custom
  ;; Auto-completion.
  (corfu-auto t)
  (corfu-auto-delay 0.3)
  :config
  ;; Free the `RET' key for less intrusive behavior.
  (keymap-unset corfu-map "RET")
  :hook
  (after-init . global-corfu-mode))

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
  ;; Avoid re-indenting current line after entering `RET'.
  (setopt electric-indent-inhibit t)
  :hook
  (before-save . delete-trailing-whitespace)
  ;; Use Ctrl-C/X/Z for copy, cut, paste.
  (after-init . cua-mode)
  ;; Automatic parenthesis pairing.
  (after-init . electric-pair-mode))

;; Check syntax on the fly.
;;guix:emacs-flycheck
(use-package flycheck
  :hook
  (after-init . global-flycheck-mode))

;;guix:emacs-flycheck-guile
(use-package flycheck-guile
  :after (flycheck geiser-guile))

;;guix:emacs-geiser
(use-package geiser
  :custom
  (geiser-autodoc-identifier-format "%s → %s")
  (geiser-default-implementation 'guile)
  (geiser-active-implementation '(guile))
  (geiser-mode-smart-tab-p t)
  (geiser-repl-query-on-kill-p nil)
  :init
  ;; Context menu on right click.
  ;; Taken from guile-studio: https://elephly.net/guile-studio/
  (defun context-menu ()
    (let ((menu (make-sparse-keymap)))
      (pcase major-mode
        ('geiser-repl-mode
         (define-key menu (vector 'insert-image)
                     '("Insert image" . geiser--guile-picture-language--pict-from-file))
         menu)
        ('scheme-mode
         (define-key menu (vector 'switch-to-repl)
                     '("Switch to REPL" . geiser-repl-switch))
         (define-key menu (vector 'eval-buffer)
                     '("Evaluate buffer" . geiser-eval-buffer))
         (define-key menu (vector 'lookup-documentation)
                     '("Show documentation". geiser-doc-symbol-at-point))
         menu)
        (_
         (mouse-menu-major-mode-map)))))
  :bind
  ([mouse-3] . (lambda (event)
                 (interactive "e")
                 (mouse-set-point event)
                 (popup-menu (context-menu)))))

;;guix:emacs-geiser-guile
(use-package geiser-guile
  :after (geiser)
  :config
  ;; TODO: Make `flycheck-guile' support `guix repl'.
  (dolist (path
           (mapcar
            #'expand-file-name
            '("~/.config/guix/current/lib/guile/3.0/site-ccache"
              "~/.config/guix/current/share/guile/site/3.0"
              "~/.guix-profile/lib/guile/3.0/site-ccache"
              "~/.guix-profile/share/guile/site/3.0"
              "~/.guix-home/profile/lib/guile/3.0/site-ccache"
              "~/.guix-home/profile/share/guile/site/3.0"
              "/run/current-system/profile/lib/guile/3.0/site-ccache"
              "/run/current-system/profile/share/guile/site/3.0")))
    (add-to-list 'geiser-guile-load-path path t)))

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
  (git-commit-cd-to-toplevel t))

(use-package project
  :config
  (add-to-list 'project-switch-commands '(magit-project-status "Magit") t)
  :bind
  (:map project-prefix-map
        ("m" . magit-project-status)))

;;guix:emacs-forge
(use-package forge
  :after (magit))


;;;
;;; Set up initial screen.
;;;

(progn
  (setopt initial-scratch-message
          ";;; Type your Guile program here and evaluate it.\n\n")
  (scheme-mode)
  (geiser-repl-switch)
  (geiser-repl-import-module "(rosenthal)")
  (geiser-repl-import-module "(nonguix transformations)")
  (delete-window)
  (display-splash-screen))
