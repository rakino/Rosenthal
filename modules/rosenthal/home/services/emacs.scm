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
of Emacs extensions."))

(define home-emacs-package
  (match-record-lambda <home-emacs-configuration>
      (emacs packages)
    (let* ((home-emacs-profile
            (profile
              (name "home-emacs-profile")
              (content (manifest
                        (cons (package->manifest-entry emacs)
                              (manifest-entries packages))))))
           (home-emacs-program
            (program-file "home-emacs-program"
              (with-imported-modules
                  ;; XXX: (guix profiles) imports (guix config).
                  (source-module-closure
                   '((guix profiles)
                     (guix build utils)))
                #~(begin
                    (use-modules (ice-9 match)
                                 (guix profiles)
                                 (guix build utils))
                    (let ((profile #$home-emacs-profile))
                      (load-profile profile)
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

(define home-emacs-shepherd
  (list (shepherd-service
          (documentation "Run Emacs daemon.")
          (provision '(emacs-daemon))
          (modules '((shepherd support)))
          (start
           #~(make-forkexec-constructor
              '("emacs" "--fg-daemon")
              #:log-file (in-vicinity %user-log-dir "emacs-daemon.log")))
          (stop #~(make-kill-destructor)))))

(define home-emacs-service-type
  (service-type
    (name 'home-emacs)
    (extensions
     (list (service-extension home-profile-service-type
                              (compose list home-emacs-package))
           (service-extension home-shepherd-service-type
                              (const home-emacs-shepherd))))
    (default-value (home-emacs-configuration))
    (description "Install Emacs into home profile and run its daemon.")))
