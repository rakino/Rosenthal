;;; -*- lexical-binding: t -*-

(use-package emacs
  :custom
  ;; Disable tab indentation.
  (indent-tabs-mode nil)
  ;; CJK support.
  (word-wrap-by-category t)
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
  (geiser-repl-query-on-kill-p nil)
  :init
  ;; Context menu on right click.
  ;; Taken from guile-studio: https://elephly.net/guile-studio/
  (defun context-menu ()
    (let ((menu (make-sparse-keymap)))
      (pcase major-mode
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

(use-package geiser-guile
  :after (geiser)
  :custom
  (geiser-active-implementation '(guile))
  (geiser-default-implementation 'guile)
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

(use-package macrostep
  :bind
  ("C-c e" . macrostep-expand))

(use-package macrostep-geiser
  :after (geiser-mode)
  :hook
  (geiser-mode . macrostep-geiser-setup))

(use-package macrostep-geiser
  :after (geiser-repl)
  :hook
  (geiser-repl-mode . macrostep-geiser-setup))

(use-package puni
  :hook
  ((eval-expression-minibuffer-setup nxml-mode prog-mode sgml-mode tex-mode)
   . puni-mode))
