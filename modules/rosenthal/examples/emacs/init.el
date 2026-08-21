;;; -*- lexical-binding: t -*-

(setopt custom-file (locate-user-emacs-file "custom.el"))
(if (not (file-exists-p custom-file))
    (make-empty-file custom-file)
  (load custom-file))

;; Keep ~/.config/emacs clean by setting default storage locations for various
;; packages.  Set this up first.
(use-package no-littering
  :config
  (no-littering-theme-backups))

(use-package emacs
  :custom
  (fill-column 80)
  (shell-file-name "/bin/sh")           ;use POSIX-compatible shell
  (word-wrap-by-category t)             ;improve CJK word-wrapping
  (pixel-scroll-precision-interpolate-page t)
  (show-paren-context-when-offscreen 'overlay)
  :config
  (setopt electric-indent-inhibit t)    ;disable automatic re-indentation
  :hook
  (after-init . show-paren-mode)
  (before-save . delete-trailing-whitespace)
  (prog-mode . display-fill-column-indicator-mode))


;;;
;;; Guile & Guix hacking.
;;;

(use-package flycheck
  :hook
  (after-init . global-flycheck-mode))

(use-package flycheck-guile
  :after flycheck geiser-guile)

(use-package geiser
  :custom
  (geiser-active-implementation '(guile))
  (geiser-default-implementation 'guile)
  (geiser-autodoc-identifier-format "%s → %s")
  (geiser-mode-smart-tab-p t)
  (geiser-mode-start-repl-p t)
  (geiser-repl-query-on-kill-p nil))

(use-package geiser-guile
  :after geiser
  :config
  (setopt geiser-guile-load-path
          (let* ((cmd "echo '(write %load-path)' | guix repl -q --type=machine")
                 (out (nth 1 (split-string (shell-command-to-string cmd) "\n"))))
            (read out))))

(use-package guix
  :hook
  (scheme-mode . guix-devel-mode))

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

(use-package macrostep
  :bind
  ((:map emacs-lisp-mode-map)
   ("C-c e" . macrostep-expand)))

(use-package macrostep-geiser
  :after geiser
  :hook
  ((geiser-mode geiser-repl-mode) . macrostep-geiser-setup)
  :bind
  ((:map geiser-mode-map)
   ("C-c e" . macrostep-expand))
  ((:map geiser-repl-mode-map)
   ("C-c e" . macrostep-expand)))

(use-package parinfer-rust-mode
  :config
  ;; Disable parenthesis pairing in `electric-pair-mode' when
  ;; `parinfer-rust-mode' is active.
  (setopt electric-pair-inhibit-predicate
          (lambda (char)
            (or (and (bound-and-true-p parinfer-rust-mode)
                     (memql char '(?\( ?\[ ?\{)))
                (electric-pair-default-inhibit char))))
  (setopt parinfer-rust-troublesome-modes
          (delq 'electric-pair-mode parinfer-rust-troublesome-modes))
  ;; These customizations are managed by Guix but will be overridden if using
  ;; `no-littering'.  Reset them to standard values.
  (custom-reevaluate-setting 'parinfer-rust-library-directory)
  (custom-reevaluate-setting 'parinfer-rust-library)
  :hook
  ;; XXX: Enable mode first, workaround to support `menu-find-file-existing'.
  ((emacs-lisp-mode lisp-mode scheme-mode) . parinfer-rust-mode-enable)
  ((emacs-lisp-mode lisp-mode scheme-mode) . parinfer-rust-mode))


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
  (after-init . vertico-mode))

(use-package vertico-directory
  :after vertico
  :hook
  ;; Tidy shadowed file names.
  (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :bind
  ((:map vertico-map)
   ("RET" . vertico-directory-enter)
   ("DEL" . vertico-directory-delete-char)
   ("M-DEL" . vertico-directory-delete-word)))


;;;
;;; Vim-like editing experience.
;;;

(use-package evil
  :custom
  (evil-undo-system 'undo-redo)
  (evil-want-integration t)
  (evil-want-keybinding nil))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))


;;;
;;; Enhancements to the default interface.
;;;

(use-package eldoc-box
  :hook
  (eldoc-mode . eldoc-box-hover-mode))

;; ElDoc integration for flycheck:
;; https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc
(use-package flycheck
  :init
  (defun mp/flycheck-eldoc (callback &rest _ignored)
    "Print flycheck messages at point by calling CALLBACK."
    (when-let ((flycheck-errors (and flycheck-mode (flycheck-overlay-errors-at (point)))))
      (mapc (lambda (err)
              (funcall callback
                       (format "%s: %s"
                               (let ((level (flycheck-error-level err)))
                                 (pcase level
                                   ('info (propertize "I" 'face 'flycheck-error-list-info))
                                   ('error (propertize "E" 'face 'flycheck-error-list-error))
                                   ('warning (propertize "W" 'face 'flycheck-error-list-warning))
                                   (_ level)))
                               (flycheck-error-message err))
                       :thing (or (flycheck-error-id err)
                                  (flycheck-error-group err))
                       :face 'font-lock-doc-face))
            flycheck-errors)))
  (defun mp/flycheck-prefer-eldoc ()
    (add-hook 'eldoc-documentation-functions #'mp/flycheck-eldoc nil t)
    (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
    (setq flycheck-display-errors-function nil)
    (setq flycheck-help-echo-function nil))
  :hook
  (flycheck-mode . mp/flycheck-prefer-eldoc))

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
  (isearch-lazy-count t)
  (isearch-wrap-pause 'no-ding))

(use-package mwim
  :bind
  ([remap move-beginning-of-line] . mwim-beginning)
  ([remap move-end-of-line] . mwim-end))

(use-package which-key
  :config
  (which-key-setup-side-window-right-bottom))


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
;;; User options for newcomers.
;;; https://cgit.git.savannah.gnu.org/cgit/emacs.git/tree/etc/themes/newcomers-presets-theme.el
;;; https://github.com/emacs-mirror/emacs/blob/master/etc/themes/newcomers-presets-theme.el
;;;

(use-package emacs
  :custom
  ;; Appearance-related options
  (font-use-system-font t)
  (frame-resize-pixelwise t)
  (window-resize-pixelwise t)
  (mode-line-compact 'long)
  ;; Mouse-related options
  (context-menu-mode t)
  (save-interprogram-paste-before-kill t)
  (mouse-yank-at-point t)
  (pixel-scroll-mode t)
  (pixel-scroll-precision-mode t) ;; see bug#69972
  (mouse-drag-and-drop-region t)
  (mouse-drag-and-drop-region-cross-program t)
  (mouse-drag-mode-line-buffer t)
  (global-xref-mouse-mode t)
  ;; Persistence-related options
  (savehist-mode t)
  (save-place-mode t)
  (recentf-mode t)
  ;; Editing-related options
  (electric-pair-mode t)
  (repeat-mode t)
  (delete-selection-mode t)
  (editorconfig-mode t)
  (indent-tabs-mode nil)
  (imenu-auto-rescan t)
  (view-read-only t)
  (column-number-mode t)
  ;; Directory managment-related options
  (dired-auto-revert-buffer t)
  (dired-mouse-drag-files t)
  (shell-command-prompt-show-cwd t)
  ;; File-related options
  ;; (etags-regen-mode t)
  (vc-auto-revert-mode t)
  (vc-deduce-backend-nonvc-modes t)
  (vc-dir-save-some-buffers-on-revert t)
  (vc-find-revision-no-save t)
  (vc-follow-symlinks t)
  (vc-use-incoming-outgoing-prefixes t)
  ;; Completion-related options
  (minibuffer-visible-completions t)
  (completions-detailed t)
  (completions-group t)
  (completion-auto-select 'second-tab)
  (completion-eager-update t)
  ;; (completion-styles '(basic emacs22 flex))
  ;; (global-completion-preview-mode t)
  (tab-always-indent 'complete)
  (which-key-mode t)
  ;; Package-related options
  (package-autosuggest-mode t)
  (package-menu-use-current-if-no-marks nil)
  ;; Frame- and window-related options
  (frame-inhibit-implied-resize t)
  (tab-bar-history-mode t)
  (tab-bar-show t)
  ;; Programming-related options
  (compilation-scroll-output 'first-error)
  :hook
  (prog-mode . display-line-numbers-mode)
  ;; (prog-mode . flymake-mode)
  ;; (prog-mode . flyspell-prog-mode)
  (text-mode . display-line-numbers-mode))
  ;; (text-mode . flyspell-mode))


;;;
;;; Font configuration with proper CJK support.  Based on
;;; https://github.com/nykma/nema/blob/develop/my-sample/font.el
;;;

(use-package emacs
  :config
  (defun hako/load-font ()
    (let* ((size     16)
           (mono     (font-spec :family "Victor Mono"      :size size))
           (modeline (font-spec :family "Source Serif 4"   :size size))
           (cjk      (font-spec :family "Sarasa Mono CL"   :size size))
           (symbol   (font-spec :family "Noto Color Emoji" :size size))
           (emoji    (font-spec :family "Noto Color Emoji" :size size)))
      (when (display-graphic-p)
        (set-face-attribute 'default            nil :font mono)
        (set-face-attribute 'mode-line          nil :font modeline)
        (set-face-attribute 'mode-line-active   nil :font modeline)
        (set-face-attribute 'mode-line-inactive nil :font modeline)
        (let ((font (frame-parameter nil 'font)))
          (dolist (charset '(kana han hangul cjk-misc bopomofo))
            (set-fontset-font font charset cjk))
          (set-fontset-font font 'symbol symbol nil 'append)
          (set-fontset-font font 'emoji  emoji  nil 'append)))))

  (if (daemonp)
      (add-hook 'server-after-make-frame-hook #'hako/load-font)
    (add-hook 'after-init-hook #'hako/load-font)))


;;;
;;; Set up a ready-to-use geiser REPL.
;;;

(progn
  (scheme-mode)
  (dolist (module
           '("(guix)"
             "(gnu)"
             "(gnu services cups)"
             "(gnu services desktop)"
             "(gnu services guix)"
             "(gnu services networking)"
             "(gnu services shepherd)"
             "(gnu services ssh)"
             "(gnu services xorg)"
             "(gnu home)"
             "(gnu home services)"
             "(gnu home services desktop)"
             "(gnu home services shepherd)"))
    (geiser-repl-import-module module))
  (delete-window))
