;;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal ci tarball)
  #:use-module (guix derivations)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix store)
  #:use-module (guix scripts pack)
  #:use-module (gnu ci)
  #:use-module (gnu compression)
  #:use-module (rosenthal packages package-management)
  #:export (cuirass-jobs))

;; Adapted from (@@ (gnu ci) tarball-jobs).
(define (cuirass-jobs store arguments)
  "Return jobs to build the self-contained Guix binary tarball."
  (define (->job name drv)
    (let ((name (string-append name "." system)))
      (parameterize ((%graft? #f))
        (derivation->job name drv))))

  (map (lambda (system)
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
                  #:system system)))
       (arguments->systems arguments)))
