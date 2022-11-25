;; SPDX-FileCopyrightText: 2022 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages xdisorg)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages xdisorg))

(define-public libdrm-2.4.114
  (let ((base libdrm))
    (package
      (inherit base)
      (name "libdrm")
      (version "2.4.114")
      (source (origin
                (method url-fetch)
                (uri (string-append "https://dri.freedesktop.org/libdrm/"
                                    "libdrm-" version ".tar.xz"))
                (sha256
                 (base32
                  "09nhk3jx3qzggl5vyii3yh4zm0npjqsbxhzvxrg2xla77a2cyj9h")))))))

(define-public libxkbcommon-1.4.1
  (package
    (inherit libxkbcommon)
    (name "libxkbcommon")
    (version "1.4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://xkbcommon.org/download/libxkbcommon-"
                    version ".tar.xz"))
              (sha256
               (base32
                "0fbb2dyjvf71p42y2jmwdcylsvj03w52f5rb23c2d00rwahhfg4l"))))))

(define-public libxkbcommon-minimal
  (let ((base libxkbcommon-1.4.1))
    (package
      (inherit base)
      (name "libxkbcommon-minimal")
      (arguments
       (list #:configure-flags
             #~(list "-Denable-x11=false"
                     "-Denable-wayland=false"
                     (string-append "-Dxkb-config-root="
                                    (search-input-directory
                                     %build-inputs "/share/X11/xkb")))))
      (inputs
       (modify-inputs (package-inputs base)
         (delete "libx11" "libxcb" "wayland" "wayland-protocols"))))))
