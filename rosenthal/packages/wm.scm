;; SPDX-FileCopyrightText: 2022 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages wm)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages zig)
  #:use-module (rosenthal packages admin)
  #:use-module (rosenthal packages freedesktop)
  #:use-module (rosenthal packages xdisorg))

(define-public wlroots-0.16.0
  (let ((base wlroots))
    (package
      (inherit base)
      (name "wlroots")
      (version "0.16.0")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.freedesktop.org/wlroots/wlroots.git")
                      (commit version)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "18rfr3wfm61dv9w8m4xjz4gzq2v3k5vx35ymbi1cggkgbk3lbc4k"))))
      (build-system meson-build-system)
      (arguments
       (list #:build-type "release"
             #:phases
             #~(modify-phases %standard-phases
                 (add-after 'unpack 'fix-hwdata-path
                   (lambda* (#:key inputs #:allow-other-keys)
                     (substitute* "backend/drm/meson.build"
                       (("/usr/\\<(share/hwdata/pnp\\.ids)\\>" all path)
                        (search-input-file inputs path))))))))
      (propagated-inputs
       (modify-inputs (package-propagated-inputs base)
         (append libdrm-2.4.114)
         (replace "libseat" libseat-sans-logind)
         (replace "libxkbcommon" libxkbcommon-minimal)
         (replace "pixman" pixman-0.42.2)
         (replace "wayland" wayland-1.21.0)
         (replace "wayland-protocols" wayland-protocols-1.30)))
      (native-inputs (list `(,hwdata "pnp") pkg-config)))))

(define-public wlroots-dev
  (let ((base wlroots-0.16.0)
        (revision "86")
        (commit "060df4c6c0f92e3989ce6fa13e5862bb3bee7dae"))
    (package
      (inherit base)
      (name "wlroots-dev")
      (version (git-version "0.16.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.freedesktop.org/wlroots/wlroots.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0d8pisxza8fwcvz5kl04svaffzka3nfcl8pnyiky0ihf59p45pik")))))))
