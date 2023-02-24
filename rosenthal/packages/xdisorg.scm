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

(define-public libdrm-2.4.115
  (let ((base libdrm))
    (package
      (inherit base)
      (name "libdrm")
      (version "2.4.115")
      (source (origin
                (method url-fetch)
                (uri (string-append "https://dri.freedesktop.org/libdrm/"
                                    "libdrm-" version ".tar.xz"))
                (sha256
                 (base32
                  "1sv53x41c1hws8c65mavjlmjhgpwpfzhagjf3cwxpga20pzgnk2m")))))))

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

(define-public pixman-0.42.2
  (package
    (inherit pixman)
    (name "pixman")
    (version "0.42.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.cairographics.org/releases/"
                                  "pixman-" version ".tar.gz"))
              (sha256
               (base32
                "0pk298iqxqr64vk3z6nhjwr6vjg1971zfrjkqy5r9zd2mppq057a"))))))
