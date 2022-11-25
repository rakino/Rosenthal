;; SPDX-FileCopyrightText: 2022 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages virtualization)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (gnu packages virtualization))

;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=59114
(define-public bubblewrap-0.7.0
  (let ((base bubblewrap))
    (package
      (inherit base)
      (name "bubblewrap")
      (version "0.7.0")
      (source (origin
                (inherit (package-source base))
                (method url-fetch)
                (uri (string-append "https://github.com/containers/bubblewrap/"
                                    "releases/download/v" version
                                    "/bubblewrap-" version ".tar.xz"))
                (sha256
                 (base32
                  "1p59hawgpf16mc01ybf6dfb2b96pk7h65ls0si9yldyh1c8bfjkn")))))))
