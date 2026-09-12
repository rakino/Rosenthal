;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2012-2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013,2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2016 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2017 Alex Kost <alezost@gmail.com>
;;; Copyright © 2018-2020 Caleb Ristvedt <caleb.ristvedt@cune.org>
;;; Copyright © 2020-2023,2025 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 Miguel Ángel Arruga Vivas <rosen644835@gmail.com>
;;; Copyright © 2020 Christine Lemmer-Webber <cwebber@dustycloud.org>
;;; Copyright © 2022 Liliana Marie Prikler <liliana.prikler@gmail.com>
;;; Copyright © 2022 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2022 Brian Cully <bjc@kublai.com>
;;; Copyright © 2023 (unmatched-parenthesis <paren@disroot.org>
;;; Copyright © 2024 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2024 Tomas Volf <~@wolfsden.cz>
;;; Copyright © 2024 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2025 David Elsing <david.elsing@posteo.net>
;;; Copyright © 2025 Hilton Chain <hako@ultrarare.space>

;; Per-directory local variables for GNU Emacs 23 and later.
((nil
  . ((fill-column . 78)
     (tab-width   .  8)
     (sentence-end-double-space . t)

     ;; For use with 'bug-reference-prog-mode'.
     (bug-reference-url-format . "https://codeberg.org/hako/Rosenthal/issues/%s")
     (bug-reference-bug-regexp
      . "\\(#\\([0-9]+\\)\\)")

     (geiser-insert-actual-lambda . nil)

     (eval . (add-to-list 'completion-ignored-extensions ".go"))))

 (c-mode          . ((c-file-style . "gnu")))
 (emacs-lisp-mode . ((indent-tabs-mode . nil)))
 (texinfo-mode    . ((indent-tabs-mode . nil)
                     (fill-column . 72)))
 (scheme-mode
  .
  ((geiser-repl-add-project-paths . ("modules"))

   (eval . (put 'computed-substitution-with-inputs 'scheme-indent-function 1))
   (eval . (put 'hidden-desktop-entry 'scheme-indent-function 1))
   (eval . (put 'modify-services/by-name 'scheme-indent-function 1))
 ;; Nonguix
   ;; Note this next setting will use the current guix as the geiser binary;
   ;; one working with a local guix checkout may want something different.
   (geiser-guile-binary . ("guix" "repl"))

   (eval . (put 'with-transformation 'scheme-indent-function 1))

   ;; Below are copied from Guix.
   (indent-tabs-mode . nil)

   ;; Emacs 28 changed the behavior of 'lisp-fill-paragraph', which causes the
   ;; first line of package descriptions to extrude past 'fill-column', and
   ;; somehow that is deemed more correct upstream (see:
   ;; https://issues.guix.gnu.org/56197).
   (eval . (progn
             (require 'lisp-mode)
             (defun emacs27-lisp-fill-paragraph (&optional justify)
               (interactive "P")
               (or (fill-comment-paragraph justify)
                   (let ((paragraph-start
                          (concat paragraph-start
                                  "\\|\\s-*\\([(;\"]\\|\\s-:\\|`(\\|#'(\\)"))
                         (paragraph-separate
                          (concat paragraph-separate "\\|\\s-*\".*[,\\.]$"))
                         (fill-column (if (and (integerp emacs-lisp-docstring-fill-column)
                                               (derived-mode-p 'emacs-lisp-mode))
                                          emacs-lisp-docstring-fill-column
                                        fill-column)))
                     (fill-paragraph justify))
                   ;; Never return nil.
                   t))
             (setq-local fill-paragraph-function #'emacs27-lisp-fill-paragraph)))

   ;; This notably allows '(' in Paredit to not insert a space when the
   ;; preceding symbol is one of these.
   (eval . (modify-syntax-entry ?~ "'"))
   (eval . (modify-syntax-entry ?$ "'"))
   (eval . (modify-syntax-entry ?+ "'"))

   (eval . (put 'eval-when 'scheme-indent-function 1))
   (eval . (put 'call-with-prompt 'scheme-indent-function 1))
   (eval . (put 'test-assert 'scheme-indent-function 1))
   (eval . (put 'test-assertm 'scheme-indent-function 1))
   (eval . (put 'test-equalm 'scheme-indent-function 1))
   (eval . (put 'test-equal 'scheme-indent-function 1))
   (eval . (put 'test-eq 'scheme-indent-function 1))
   (eval . (put 'call-with-input-string 'scheme-indent-function 1))
   (eval . (put 'call-with-port 'scheme-indent-function 1))
   (eval . (put 'guard 'scheme-indent-function 1))
   (eval . (put 'lambda* 'scheme-indent-function 1))
   (eval . (put 'substitute* 'scheme-indent-function 1))
   (eval . (put 'match-record 'scheme-indent-function 3))
   (eval . (put 'match-record-lambda 'scheme-indent-function 2))

   ;; TODO: Contribute these to Emacs' scheme-mode.
   (eval . (put 'let-keywords 'scheme-indent-function 3))

   ;; 'modify-inputs' and its keywords.
   (eval . (put 'modify-inputs 'scheme-indent-function 1))
   (eval . (put 'replace 'scheme-indent-function 1))

   ;; 'modify-phases' and its keywords.
   (eval . (put 'modify-phases 'scheme-indent-function 1))
   (eval . (put 'replace 'scheme-indent-function 1))
   (eval . (put 'add-before 'scheme-indent-function 2))
   (eval . (put 'add-after 'scheme-indent-function 2))

   (eval . (put 'modify-services 'scheme-indent-function 1))
   (eval . (put 'with-directory-excursion 'scheme-indent-function 1))
   (eval . (put 'with-file-lock 'scheme-indent-function 1))
   (eval . (put 'with-file-lock/no-wait 'scheme-indent-function 1))
   (eval . (put 'with-profile-lock 'scheme-indent-function 1))
   (eval . (put 'with-writable-file 'scheme-indent-function 2))

   (eval . (put 'package/inherit 'scheme-indent-function 1))
   (eval . (put 'substitute-keyword-arguments 'scheme-indent-function 1))
   (eval . (put 'with-store 'scheme-indent-function 1))
   (eval . (put 'with-store/non-blocking 'scheme-indent-function 1))
   (eval . (put 'with-external-store 'scheme-indent-function 1))
   (eval . (put 'with-error-handling 'scheme-indent-function 0))
   (eval . (put 'with-mutex 'scheme-indent-function 1))
   (eval . (put 'with-atomic-file-output 'scheme-indent-function 1))
   (eval . (put 'call-with-compressed-output-port 'scheme-indent-function 2))
   (eval . (put 'call-with-decompressed-port 'scheme-indent-function 2))
   (eval . (put 'call-with-gzip-input-port 'scheme-indent-function 1))
   (eval . (put 'call-with-gzip-output-port 'scheme-indent-function 1))
   (eval . (put 'call-with-lzip-input-port 'scheme-indent-function 1))
   (eval . (put 'call-with-lzip-output-port 'scheme-indent-function 1))
   (eval . (put 'signature-case 'scheme-indent-function 1))
   (eval . (put 'emacs-batch-eval 'scheme-indent-function 0))
   (eval . (put 'emacs-batch-edit-file 'scheme-indent-function 1))
   (eval . (put 'emacs-substitute-sexps 'scheme-indent-function 1))
   (eval . (put 'emacs-substitute-variables 'scheme-indent-function 1))
   (eval . (put 'with-derivation-narinfo 'scheme-indent-function 1))
   (eval . (put 'with-derivation-substitute 'scheme-indent-function 2))
   (eval . (put 'with-status-report 'scheme-indent-function 1))
   (eval . (put 'with-status-verbosity 'scheme-indent-function 1))
   (eval . (put 'with-build-handler 'scheme-indent-function 1))

   (eval . (put 'mlambda 'scheme-indent-function 1))
   (eval . (put 'mlambdaq 'scheme-indent-function 1))
   (eval . (put 'syntax-parameterize 'scheme-indent-function 1))
   (eval . (put 'with-monad 'scheme-indent-function 1))
   (eval . (put 'mbegin 'scheme-indent-function 1))
   (eval . (put 'mwhen 'scheme-indent-function 1))
   (eval . (put 'munless 'scheme-indent-function 1))
   (eval . (put 'mlet* 'scheme-indent-function 2))
   (eval . (put 'mlet 'scheme-indent-function 2))
   (eval . (put 'state-parameterize 'scheme-indent-function 2))
   (eval . (put 'store-parameterize 'scheme-indent-function 2))
   (eval . (put 'run-with-store 'scheme-indent-function 1))
   (eval . (put 'run-with-state 'scheme-indent-function 1))
   (eval . (put 'wrap-program 'scheme-indent-function 1))
   (eval . (put 'wrap-script 'scheme-indent-function 1))
   (eval . (put 'with-imported-modules 'scheme-indent-function 1))
   (eval . (put 'with-extensions 'scheme-indent-function 1))
   (eval . (put 'with-parameters 'scheme-indent-function 1))
   (eval . (put 'let-system 'scheme-indent-function 1))
   (eval . (put 'with-build-variables 'scheme-indent-function 2))

   (eval . (put 'with-database 'scheme-indent-function 2))
   (eval . (put 'call-with-database 'scheme-indent-function 1))
   (eval . (put 'call-with-transaction 'scheme-indent-function 1))
   (eval . (put 'call-with-retrying-transaction 'scheme-indent-function 1))

   (eval . (put 'call-with-container 'scheme-indent-function 1))
   (eval . (put 'container-excursion 'scheme-indent-function 1))
   (eval . (put 'eventually 'scheme-indent-function 1))

   (eval . (put 'call-with-progress-reporter 'scheme-indent-function 1))
   (eval . (put 'with-repository 'scheme-indent-function 2))
   (eval . (put 'with-temporary-git-repository 'scheme-indent-function 2))
   (eval . (put 'with-environment-variables 'scheme-indent-function 1))
   (eval . (put 'with-fresh-gnupg-setup 'scheme-indent-function 1))

   (eval . (put 'with-paginated-output-port 'scheme-indent-function 1))

   (eval . (put 'with-shepherd-action 'scheme-indent-function 3))

   (eval . (put 'with-http-server 'scheme-indent-function 1)))))
