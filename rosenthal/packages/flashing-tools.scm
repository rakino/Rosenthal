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
        (revision "1165")
        (commit "7ffa626d1278a167c430c55891bbb6f979a5ab92"))
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
                  "015s56ygl4bp4awvxnx589xqc9mlxmyr0r8dk1rgy8zp2wbwhknh"))))
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:make-flags _)
          #~(list (string-append "CC=" #$(cc-for-target))
                  (string-append "PREFIX=" #$output)
                  "CONFIG_ENABLE_LIBUSB0_PROGRAMMERS=no")))))))
