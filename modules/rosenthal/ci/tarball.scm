;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2025 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal ci tarball)
  ;; Guile builtins
  #:use-module (srfi srfi-1)
  ;; Utilities
  #:use-module (gnu ci)
  #:use-module (gnu compression)
  #:use-module (guix derivations)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix scripts pack)
  #:use-module (guix store)
  ;; Guix packages
  #:use-module (rosenthal packages package-management)
  #:export (cuirass-jobs))

;; Copied from (@@ (gnu ci) tarball-jobs).
(define (tarball-jobs store system)
  "Return jobs to build the self-contained Guix binary tarball."
  (define (->job name drv)
    (let ((name (string-append name "." system)))
      (parameterize ((%graft? #f))
        (derivation->job name drv))))

  (list
   (->job "binary-tarball"
          (run-with-store store
            (mbegin %store-monad
              (set-guile-for-build (default-guile))
              (>>= (profile-derivation (packages->manifest (list guix/dolly)))
                   (lambda (profile)
                     (self-contained-tarball "guix-binary" profile
                                             #:profile-name "current-guix"
                                             #:localstatedir? #t
                                             #:compressor
                                             (lookup-compressor "xz")))))
            #:system system))))

;; Copied from (@@ (gnu ci) cuirass-jobs).
(define (cuirass-jobs store arguments)
  "Register Cuirass jobs."

  (define systems
    (arguments->systems arguments))

  ;; Turn off grafts.  Grafting is meant to happen on the user's machines.
  (parameterize ((%graft? #f))
    ;; Return one job for each package, except bootstrap packages.
    (append-map
     (lambda (system)
       (format (current-error-port)
               "evaluating for '~a' (heap size: ~a MiB)...~%"
               system
               (round
                (/ (assoc-ref (gc-stats) 'heap-size)
                   (expt 2. 20))))
       (invalidate-derivation-caches!)
       (tarball-jobs store system))
     systems)))
