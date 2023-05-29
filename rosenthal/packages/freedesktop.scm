;; SPDX-FileCopyrightText: 2022 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages freedesktop)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages freedesktop))

(define-public wayland-protocols-1.31
  (let ((base wayland-protocols))
    (package
      (inherit base)
      (name "wayland-protocols")
      (version "1.31")
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://gitlab.freedesktop.org/wayland/wayland-protocols"
                      "/-/releases/" version "/downloads/"
                      "wayland-protocols-" version ".tar.xz"))
                (sha256
                 (base32
                  "0f72359fzvh6jzri4fd79m34rwm2r55p2ryq4306wrw7xliafzx0")))))))
