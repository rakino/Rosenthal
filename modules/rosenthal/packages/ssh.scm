;; SPDX-FileCopyrightText: 2022 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages ssh)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ssh))

(define-public dropbear/static
  (let ((base dropbear))
    (package
      (inherit dropbear)
      (name "dropbear-static")
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:configure-flags flags)
          #~(append #$flags (list "--enable-static")))))
      (inputs
       (modify-inputs (package-inputs base)
         (append `(,zlib "static"))
         (replace "libtomcrypt" `(,libtomcrypt "static"))
         (replace "libtommath" `(,libtommath "static")))))))
