;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2025-2026 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal packages wm)
  ;; Utilities
  #:use-module (guix deprecation)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  ;; Guix origin methods
  #:use-module (guix git-download)
  ;; Guix build systems
  #:use-module (guix build-system meson)
  ;; Guix packages
  #:use-module (gnu packages calendar)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages markup)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages stb)
  #:use-module (gnu packages window-management)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml))

(define-public noctalia-greeter
  (package
    (name "noctalia-greeter")
    (version "1.7.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/noctalia-dev/noctalia-greeter")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1wsrmjh8mmnbsfsspn0hvpd6di9mlm20qiigxcniq5s6za74ic90"))))
    (build-system meson-build-system)
    (arguments
     (list #:build-type "release"
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'prepare-for-build
                 (lambda _
                   ;; Adjust import paths for STB headers packaged in Guix.
                   (substitute* (find-files "." "\\.cpp$|^meson\\.build$")
                     (("\\bstb/stb_") "stb_"))
                   ;; Reproducibility.
                   (substitute* "meson.build"
                     (("'-march=native', '-mtune=native',") "")))))))
    (native-inputs (list pkg-config wayland))
    (inputs
     (list cairo
           fontconfig
           freetype
           glib
           harfbuzz
           jemalloc
           libjxl
           (librsvg-for-system)
           libwebp
           libxkbcommon
           libxml2
           mesa
           nlohmann-json
           pango
           stb-image-resize2
           tomlplusplus
           wayland
           wayland-protocols
           wlroots))
    (home-page "https://noctalia.dev/")
    (synopsis "Login greeter for greetd")
    (description
     "Noctalia Greeter is a minimal login greeter for greetd that matches the
look and feel of Noctalia Shell")
    (license license:expat)))
