;; SPDX-FileCopyrightText: 2022 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages flashing-tools)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages flashing-tools))

(define-public flashrom-dev
  (let ((base flashrom)
        (revision "1081")
        (commit "8528ea0845171a4b26de44757d5b30c9727e8d5b"))
    (package
      (inherit base)
      (name "flashrom-dev")
      (version (git-version "1.2" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/flashrom/flashrom")
                      (commit commit)))
                (file-name (git-file-name name version ))
                (sha256
                 (base32
                  "1nycc11zbc3kay6jlqvalrvnr1yhxdvki8yxcndrhsjhs7bm7r02"))))
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:make-flags _)
          #~(list (string-append "CC=" #$(cc-for-target))
                  (string-append "PREFIX=" #$output))))))))
