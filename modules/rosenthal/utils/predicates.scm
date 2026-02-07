;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2025 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal utils predicates)
  ;; Utilities
  #:use-module (guix gexp)
  #:export (file-object?
            user-and-group-id?))

(define (file-object? val)
  (or (string? val)
      (file-like? val)))

(define (user-and-group-id? val)
  (or (integer? val)
      (eqv? val #f)))
