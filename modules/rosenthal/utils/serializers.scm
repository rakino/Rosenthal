;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © Copyright © 2022 Andrew Tropin <andrew@trop.in>

(define-module (rosenthal utils serializers)
  ;; Guile builtins
  #:use-module (srfi srfi-1)
  ;; Utilities
  #:use-module (guix gexp)
  ;; Guix System - services
  #:use-module (gnu services configuration)
  #:export (path?
            serialize-path

            string-or-gexp?
            serialize-string-or-gexp

            gexp-text-config?
            serialize-gexp-text-config)
  #:re-export (interpose))

(define path? string?)
(define (serialize-path field-name val) val)

(define (string-or-gexp? sg) (or (string? sg) (gexp? sg)))
(define (serialize-string-or-gexp field-name val) "")

;; Guix proper has a different version of text-config.
(define (gexp-text-config? config)
  (and (list? config) (every string-or-gexp? config)))
(define (serialize-gexp-text-config field-name val)
  #~(string-append #$@(interpose val "\n" 'suffix)))
