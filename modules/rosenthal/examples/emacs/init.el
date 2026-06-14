;;; -*- lexical-binding: t -*-

(setopt custom-file (locate-user-emacs-file "custom.el"))
(if (not (file-exists-p custom-file))
    (make-empty-file custom-file)
  (load custom-file))

(use-package no-littering
  :config
  (no-littering-theme-backups))

(use-package emacs
  :custom
  (fill-column 80)
  (indent-tabs-mode nil)                ;disable tab indentation
  (shell-file-name "/bin/sh")           ;use POSIX-compatible shell
  (word-wrap-by-category t)             ;improve CJK word-wrapping
  :config
  (setopt electric-indent-inhibit t)    ;disable automatic re-indentation
  :hook
  (before-save . delete-trailing-whitespace)
  (prog-mode . display-line-numbers-mode)
  (prog-mode . display-fill-column-indicator-mode))


;;;
;;; Enhancements to the default interface.
;;;

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key]      . helpful-key)
  ([remap describe-command]  . helpful-command)
  ([remap describe-symbol]   . helpful-symbol)
  ("C-c C-d" . helpful-at-point))

(use-package isearch
  :custom
  (isearch-wrap-pause 'no-ding))

(use-package mwim
  :bind
  ([remap move-beginning-of-line] . mwim-beginning)
  ([remap move-end-of-line] . mwim-end))

(use-package which-key
  :config
  (which-key-setup-side-window-right-bottom)
  :hook
  (after-init . which-key-mode))


;;;
;;; Completion.
;;;

(use-package corfu
  :custom
  (corfu-auto t)                        ;auto-complete
  (corfu-auto-delay 0.3)
  :hook
  (after-init . global-corfu-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-matching-styles '(orderless-literal orderless-flex orderless-regexp)))

(use-package vertico
  :custom
  ;; Case-insensitive completion.
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  :hook
  (after-init . savehist-mode)          ;save minibuffer history
  (after-init . vertico-mode))

(use-package vertico-directory
  :after (vertico)
  :hook
  ;; Tidy shadowed file names.
  (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :bind
  ((:map vertico-map)
   ("RET" . vertico-directory-enter)
   ("DEL" . vertico-directory-delete-char)
   ("M-DEL" . vertico-directory-delete-word)))


;;;
;;; Guile hacking.
;;;

(use-package flycheck
  :hook
  (after-init . global-flycheck-mode))

(use-package flycheck-guile
  :after (flycheck geiser-guile))

(use-package geiser
  :custom
  (geiser-autodoc-identifier-format "%s → %s")
  (geiser-mode-smart-tab-p t)
  (geiser-mode-start-repl-p t)
  (geiser-repl-query-on-kill-p nil))

(use-package geiser-guile
  :after (geiser)
  :custom
  (geiser-active-implementation '(guile))
  (geiser-default-implementation 'guile)
  :config
  ;; TODO: Add guix repl support to `flycheck-guile'.
  (dolist (path
           (mapcar
            #'expand-file-name
            '("~/.config/guix/current/share/guile/site/3.0"
              "~/.guix-profile/share/guile/site/3.0"
              "~/.guix-home/profile/share/guile/site/3.0"
              "/run/current-system/profile/share/guile/site/3.0")))
    (add-to-list 'geiser-guile-load-path path t)))

(use-package info-look
  :config
  (info-lookup-add-help
   :mode 'scheme-mode
   :regexp "[^()`',\"        \n]+"
   :ignore-case nil
   :doc-spec
   (mapcar (lambda (node-name)
             (list node-name nil "^[       ]+-+ [^:]+:[    ]*" "\\b"))
           '("(guile)R5RS Index"
             "(guix)Programming Index"
             "(guile)Procedure Index"
             "(guile)Variable Index"
             "(r5rs)Index"
             "(guile)Concept Index"
             "(guix)Concept Index"))))

(use-package parinfer-rust-mode
  :config
  ;; These customizations are managed by Guix but will be overridden if using
  ;; `no-littering'.  Reset them to standard values.
  (custom-reevaluate-setting 'parinfer-rust-library-directory)
  (custom-reevaluate-setting 'parinfer-rust-library)
  :hook
  ((emacs-lisp-mode lisp-mode scheme-mode) . parinfer-rust-mode))


;;;
;;; Theming.
;;;

(use-package emacs
  :config
  (load-theme 'modus-operandi-tinted :no-confirm))

(use-package doom-modeline
  :custom
  (doom-modeline-icon nil)
  (doom-modeline-height 18)
  :hook
  (after-init . doom-modeline-mode))


;;;
;;; Font configuration with proper CJK support.  Based on
;;; https://github.com/nykma/nema/blob/develop/my-sample/font.el
;;;

(use-package emacs
  :config
  (defvar nema--font-size 12 "Font size")
  (defvar nema-fonts '((sans     . "sans")
                       (serif    . "serif")
                       (mono     . "Victor Mono")
                       (cjk      . "Sarasa Mono CL")
                       (symbol   . "Noto Color Emoji")
                       (modeline . "Source Serif 4"))
    "Fonts to use.")

  (defun nema//get-font-family (key)
    (alist-get key nema-fonts))

  (defun nema//generate-font-spec (key)
    (format "%s-%d"
            (nema//get-font-family key)
            nema--font-size))

  (defun nema//load-base-font ()
    "Load the default font for ascii characters."
    (let* ((font-spec (nema//generate-font-spec 'mono)))
      (set-frame-parameter nil 'font font-spec)
      (add-to-list 'default-frame-alist (cons 'font font-spec))))

  (defun nema//load-face-font ()
    "Load fonts used in faces.

This function must be called after frame creation."
    (let ((mono (nema//generate-font-spec 'mono))
          (sans (nema//generate-font-spec 'sans))
          (serif (nema//generate-font-spec 'sans-serif))
          (modeline (nema//generate-font-spec 'modeline)))
      (set-face-attribute 'variable-pitch nil :font sans)
      (set-face-attribute 'variable-pitch-text nil :font serif)
      (set-face-attribute 'fixed-pitch nil :font mono)
      (set-face-attribute 'fixed-pitch-serif nil :font mono)
      (set-face-attribute 'mode-line nil :font modeline)
      (set-face-attribute 'mode-line-inactive nil :font modeline)))

  (defun nema//load-ext-font ()
    "Load fonts used for non-ascii characters.

This function must be called after frame creation."
    (let ((font (frame-parameter nil 'font))
          (font-spec-cjk (font-spec :family (nema//get-font-family 'cjk)))
          (font-spec-symbol (font-spec :family (nema//get-font-family 'symbol))))
      (dolist (charset '(kana han hangul cjk-misc bopomofo))
        (set-fontset-font font charset font-spec-cjk))
      (set-fontset-font font 'symbol font-spec-symbol)))

  (defun nema/load-font ()
    "Load all font configuration."
    (interactive)
    (when (display-graphic-p)
      (nema//load-base-font)
      (nema//load-ext-font)
      (nema//load-face-font)))

  (if (daemonp)
      (add-hook 'server-after-make-frame-hook #'nema/load-font)
    ;; Else: not in daemon
    (add-hook 'after-init-hook #'nema/load-font)))


;;;
;;; Scratch buffer.
;;;

(progn
  (setopt initial-scratch-message
          "\
;;; Type your Guile program here and evaluate it.
;;; `M-x cua-mode' to use Ctrl-C/X/Z for copy, cut, paste.
;;; `M-x evil-mode' for Vim-like experience.
\n")
  (scheme-mode)
  (geiser-repl-import-module "(gnu)")
  (geiser-repl-import-module "(guix)")
  (delete-window))
