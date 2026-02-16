;;; -*- lexical-binding: t -*-

(use-package emacs
  :custom
  (blink-cursor-mode nil)
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
  ;; Recursive minibuffers indicatior.
  (after-init . minibuffer-depth-indicate-mode)
  ;; Save minibuffer history.
  (after-init . savehist-mode)
  ;; `fill-column' indicator.
  (prog-mode . display-fill-column-indicator-mode))

;; Theming
(use-package emacs
  :custom
  (fringe-mode 0)
  (modus-themes-italic-constructs t)
  :config
  (load-theme 'modus-operandi-tinted :no-confirm))

(use-package corfu
  :custom
  ;; Auto-completion.
  (corfu-auto t)
  (corfu-auto-delay 0.3)
  :hook
  (after-init . global-corfu-mode)
  :bind
  ;; Free the `RET' key for less intrusive behavior.
  (:map corfu-map
        ("RET" . nil)))

(use-package doom-modeline
  :custom
  (doom-modeline-icon nil)
  (doom-modeline-height 18)
  :hook
  (after-init . doom-modeline-mode))

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key]      . helpful-key)
  ([remap describe-command]  . helpful-command)
  ([remap describe-symbol]   . helpful-symbol)
  ("C-c C-d" . helpful-at-point))

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
  ([remap move-beginning-of-line] . mwim-beginning)
  ([remap move-end-of-line] . mwim-end))

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
