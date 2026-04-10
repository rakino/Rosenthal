;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Hilton Chain <hako@ultrarare.space>

;; Inspired by https://codeberg.org/guix/guix/pulls/2395

(define-module (rosenthal home services emacs)
  ;; Utilities
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix records)
  #:use-module (guix search-paths)
  ;; Guix System - services
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  ;; Guix Home - services
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  ;; Guix build systems
  #:use-module (guix build-system trivial)
  ;; Guix packages
  #:autoload   (gnu packages emacs) (emacs)
  #:export (home-emacs-service-type
            home-emacs-configuration))

(define-configuration/no-serialization home-emacs-configuration
  (emacs
   (file-like emacs)
   "Emacs package to use.")
  (packages
   (manifest (manifest '()))
   "A manifest (@pxref{Writing Manifests,,, guix, GNU Guix Reference Manual})
of Emacs extensions.")
  (shepherd-requirement
   (list-of-symbols '())
   "List of services that should be started before this service."))

(define home-emacs-package
  (match-record-lambda <home-emacs-configuration>
      (emacs packages)
    (let* ((home-emacs-profile
            (profile
              (name "home-emacs-profile")
              (content (manifest
                        (cons (package->manifest-entry emacs)
                              (manifest-entries packages))))))
           (home-emacs-search-paths
            (map search-path-specification->sexp
                 (manifest-search-paths
                  (profile-content home-emacs-profile))))
           (home-emacs-program
            (program-file "home-emacs-program"
              (with-imported-modules
                  (source-module-closure
                   '((guix search-paths)
                     (guix build utils)))
                #~(begin
                    (use-modules (ice-9 match)
                                 (guix search-paths)
                                 (guix build utils))
                    (let ((profile #$home-emacs-profile))
                      ;; See also (@ (guix profiles) load-profile).
                      (for-each
                       (match-lambda
                         ((($ <search-path-specification> variable _ separator) . value)
                          (let ((current (getenv variable)))
                            (setenv variable
                                    (if current
                                        (if separator
                                            (string-append value separator current)
                                            value)
                                        value)))))
                       (evaluate-search-paths
                        (map sexp->search-path-specification
                             '#$home-emacs-search-paths)
                        (list profile)))
                      (match (command-line)
                        ((cmd . args)
                         (apply system*
                                (string-append profile "/bin/" (basename cmd))
                                args)))))))))
      (package
        (inherit emacs)
        (name "emacs-wrapper")
        (build-system trivial-build-system)
        (arguments
         (list #:modules '((guix build utils))
               #:builder
               #~(begin
                   (use-modules (ice-9 match)
                                (guix build utils))
                   (let ((bin (in-vicinity #$output "bin")))
                     (mkdir-p bin)
                     (with-directory-excursion bin
                       (for-each (lambda (name)
                                   (symlink #$home-emacs-program name))
                                 '("ctags"
                                   "ebrowse"
                                   "emacs"
                                   "emacsclient"
                                   "etags"))))
                   (for-each
                    (lambda (path)
                      (let ((src (in-vicinity #$home-emacs-profile path))
                            (dst (in-vicinity #$output path)))
                        (mkdir-p (dirname dst))
                        (symlink src dst)))
                    '("share/icons"
                      "share/info"
                      "share/man")))))
        (native-inputs '())
        (inputs '())
        (propagated-inputs '())
        (outputs '("out"))))))

(define (home-emacs-shepherd config)
  (match-record config <home-emacs-configuration>
                (shepherd-requirement)
    (let ((emacs (home-emacs-package config)))
      (list (shepherd-service
              (documentation "Run Emacs daemon.")
              (provision '(emacs-daemon))
              (requirement shepherd-requirement)
              (modules '((shepherd support)))
              (start
               #~(lambda args
                   ((make-forkexec-constructor
                     (list #$(file-append emacs "/bin/emacs") "--fg-daemon")
                     #:log-file (in-vicinity %user-log-dir "emacs-daemon.log")
                     ;; Inherit graphical session environment.
                     #:environment-variables (environ))
                    args)))
              (stop #~(make-kill-destructor)))))))

(define home-emacs-service-type
  (service-type
    (name 'home-emacs)
    (extensions
     (list (service-extension home-profile-service-type
                              (compose list home-emacs-package))
           (service-extension home-shepherd-service-type
                              home-emacs-shepherd)))
    (default-value (home-emacs-configuration))
    (description "Install Emacs into home profile and run its daemon.")))
