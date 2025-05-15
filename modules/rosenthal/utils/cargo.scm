;;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal utils cargo)
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:export (rosenthal-cargo-inputs))

(define* (rosenthal-cargo-inputs name #:key (module '(rosenthal packages rust-crates)))
  "Lookup Cargo inputs for NAME defined in MODULE, return an empty list if
unavailable."
  (let ((lookup (module-ref (resolve-interface module) 'lookup-cargo-inputs)))
    (or (lookup name)
        (begin
          (warning (G_ "no Cargo inputs available for '~a'~%") name)
          '()))))
