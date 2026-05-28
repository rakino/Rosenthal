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
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages xdisorg))

;; TODO: Unbundle dependencies under the third_party directory.
(define-public noctalia
  (let ((commit "0e4bb96a8b42abb47af67286902a52eaa628c50a")
        (revision "0"))
    (package
      (name "noctalia")
      (version (git-version "5.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/noctalia-dev/noctalia-shell")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0ymv5i8dfd10ynfh1lrr7h8ydbi3h1xf4gp2j6313i7yghcymqy8"))))
      (build-system meson-build-system)
      (arguments
       (list #:build-type "release"
             ;; FIXME: process_test fails with:
             ;; --8<---------------cut here---------------start------------->8---
             ;; stderr:
             ;; process_test: completion-only async command exit code was wrong
             ;; process_test: completion-only async command stdout was wrong
             ;; --8<---------------cut here---------------end--------------->8---
             #:tests? #f
             #:phases
             #~(modify-phases %standard-phases
                 (add-after 'unpack 'prepare-for-build
                   (lambda _
                     ;; For reproducibility.
                     (substitute* "meson.build"
                       (("'-march=native', '-mtune=native',") ""))
                     ;; /bin/sh doesn't exist in the build environment.
                     (substitute* "tests/process_test.cpp"
                       (("/bin/(sh)" _ cmd)
                        (which cmd))))))))
      (native-inputs
       (list pkg-config))
      (inputs
       (list cairo
             curl
             fontconfig
             freetype
             glib
             harfbuzz
             jemalloc
             (librsvg-for-system)
             libwebp
             libxkbcommon
             linux-pam
             mesa
             pango
             pipewire
             polkit
             sdbus-c++
             wayland
             wayland-protocols))
      (home-page "https://noctalia.dev/")
      (synopsis "Wayland shell and bar")
      (description
       "Noctalia is a lightweight Wayland shell and bar built directly on
Wayland and OpenGL ES, with no Qt or GTK dependency.")
      (license license:expat))))

(define-deprecated-package noctalia-shell noctalia)
