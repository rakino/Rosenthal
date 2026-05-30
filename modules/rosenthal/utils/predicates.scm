;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2025 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal utils predicates)
  ;; Utilities
  #:use-module (guix gexp)
  #:export (file-object?
            file-config?
            file-object-or-file-config?
            user-and-group-id?))

(define (file-object? val)
  (or (and (string? val)
           (string-prefix? "/" val))
      (file-like? val)))

(define (file-config? val)
  (or (gexp? val)
      (pair? val)))

(define (file-object-or-file-config? val)
  (or (file-object? val)
      (file-config? val)))

(define (user-and-group-id? val)
  (or (integer? val)
      (eqv? val #f)))
