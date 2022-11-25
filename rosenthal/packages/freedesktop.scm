;; SPDX-FileCopyrightText: 2022 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages freedesktop)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages freedesktop))

(define-public wayland-1.21.0
  (let ((base wayland))
    (package
      (inherit base)
      (name "wayland")
      (version "1.21.0")
      (source (origin
                (method url-fetch)
                (uri (string-append "https://gitlab.freedesktop.org/wayland/wayland"
                                    "/-/releases/" version "/downloads/"
                                    "wayland-" version ".tar.xz"))
                (sha256
                 (base32
                  "1b0ixya9bfw5c9jx8mzlr7yqnlyvd3jv5z8wln9scdv8q5zlvikd"))))
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases)
          #~(modify-phases #$phases
              ;; FIXME
              (delete 'check))))))))
