;; SPDX-FileCopyrightText: 2022 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages admin)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages admin))

(define-public libseat-sans-logind
  (let ((base libseat))
    (package
      (inherit base)
      (name "libseat-sans-logind")
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:configure-flags configure-flags)
          #~(append #$configure-flags
                    (list "-Dlibseat-logind=disabled")))))
      (propagated-inputs '()))))
