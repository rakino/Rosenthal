;;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages wm)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (rosenthal utils cargo)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg))

(define-public niri
  (package
   (name "niri")
   (version "25.02")
   (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/YaLTeR/niri")
                   (commit (string-append "v" version))))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "0vzskaalcz6pcml687n54adjddzgf5r07gggc4fhfsa08h1wfd4r"))))
   (build-system cargo-build-system)
   (arguments
    (list #:install-source? #f
          #:phases
          #~(modify-phases %standard-phases
              (add-after 'unpack 'use-guix-vendored-dependencies
                (lambda _
                  (substitute* "Cargo.toml"
                    (("# version =.*")
                     "version = \"*\"")
                    (("git.*optional")
                     "version = \"*\", optional")
                    (("^git = .*")
                     ""))))
              (add-after 'unpack 'set-environment
                (lambda _
                  (setenv "RUSTFLAGS"
                          (string-join
                           '("-C" "link-arg=-lEGL"
                             "-C" "link-arg=-lwayland-client")
                           " "))
                  (setenv "NIRI_BUILD_VERSION_STRING"
                          #$(package-version this-package))
                  ;; For tests.
                  (setenv "XDG_RUNTIME_DIR" "/tmp")))
              (add-after 'install 'install-extras
                (lambda* (#:key inputs #:allow-other-keys)
                  (substitute* "resources/niri.desktop"
                    (("niri-session")
                     (format #f "~a --dbus-daemon=~a ~a/bin/niri --session"
                             (search-input-file inputs "bin/dbus-run-session")
                             (search-input-file inputs "bin/dbus-daemon")
                             #$output)))
                  (install-file
                   "resources/niri.desktop"
                   (in-vicinity #$output "share/wayland-sessions"))
                  (install-file
                   "resources/niri-portals.conf"
                   (in-vicinity #$output "share/xdg-desktop-portal")))))))
   (native-inputs
    (list pkg-config))
   (inputs
    (cons* clang
           dbus
           libdisplay-info
           libinput-minimal
           libseat
           libxkbcommon
           mesa
           pango
           pipewire
           wayland
           (rosenthal-cargo-inputs 'niri)))
   (home-page "https://github.com/YaLTeR/niri")
   (synopsis "Scrollable-tiling Wayland compositor")
   (description
    "Niri is a scrollable-tiling Wayland compositor which arranges windows in a
scrollable format.  It is considered stable for daily use and performs most
functions expected of a Wayland compositor.")
   (license license:gpl3)
   (properties
    '((disable-updater? . #t)))))
