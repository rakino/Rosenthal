;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2025 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal utils cargo)
  ;; Guile builtins
  #:use-module (srfi srfi-26)
  ;; Guix build systems
  #:use-module (guix build-system cargo)
  #:export (rosenthal-cargo-inputs))

(define rosenthal-cargo-inputs
  (cut cargo-inputs <> #:module '(rosenthal packages rust-crates)))
