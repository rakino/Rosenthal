;;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal utils cargo)
  #:use-module (srfi srfi-26)
  #:use-module (guix build-system cargo)
  #:export (rosenthal-cargo-inputs))

(define rosenthal-cargo-inputs
  (cut cargo-inputs <> #:module '(rosenthal packages rust-crates)))
