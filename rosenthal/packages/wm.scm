;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages wm)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (rosenthal packages rust-crates))

(define rust-pipewire
  (let ((commit "fd3d8f7861a29c2eeaa4c393402e013578bb36d9")
        (revision "0"))
    (package
      (name "rust-pipewire")
      (version (git-version "0.8.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.freedesktop.org/pipewire/pipewire-rs.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1hzyhz7xg0mz8a5y9j6yil513p1m610q3j9pzf6q55vdh5mcn79v"))))
      (build-system cargo-build-system)
      (arguments
       (list #:skip-build? #t
             #:phases
             #~(modify-phases %standard-phases
                 ;; Avoid circular dependency.
                 (add-after 'unpack 'remove-dev-dependencies
                   (lambda _
                     (substitute* "libspa/Cargo.toml"
                       (("^pipewire.*") ""))))
                 (replace 'package
                   (lambda _
                     (begin
                       ;;error: invalid inclusion of reserved file name Cargo.toml.orig in package source
                       (when (file-exists? "Cargo.toml.orig")
                         (delete-file "Cargo.toml.orig"))

                       ;; Use unstable feature ‘--registry’.
                       (setenv "RUSTC_BOOTSTRAP" "1")
                       (for-each
                        (lambda (pkg)
                          (invoke "cargo" "package" "--offline" "--package" pkg
                                  "--registry" "crates-io" "-Z" "package-workspace"
                                  "--no-metadata" "--no-verify")
                          (for-each
                           (lambda (crate)
                             (invoke "tar" "xzf" crate "-C" "guix-vendor"))
                           (begin
                             (delete-file-recursively "target/package/tmp-registry")
                             (find-files "target/package" "\\.crate$")))
                          ((assoc-ref %standard-phases 'patch-cargo-checksums)))
                        '("libspa-sys" "libspa" "pipewire-sys" "pipewire"))
                       (unsetenv "RUSTC_BOOTSTRAP")))))))
      (inputs rust-pipewire-cargo-inputs)
      (home-page "https://pipewire.org/")
      (synopsis "Rust bindings for PipeWire")
      (description "This package provides Rust bindings for PipeWire.")
      (license license:expat))))

(define rust-smithay
  (let ((commit "0cd3345c59f7cb139521f267956a1a4e33248393")
        (revision "0"))
    (package
      (name "rust-smithay")
      (version (git-version "0.4.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/Smithay/smithay")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "191h87bpzg0l1ihfb4hmx00b86pfb5mwwc6s8i49al0vigc14l37"))))
      (build-system cargo-build-system)
      (arguments
       (list #:skip-build? #t
             #:phases
             #~(modify-phases %standard-phases
                 (replace 'package
                   (lambda _
                     (begin
                       ;;error: invalid inclusion of reserved file name Cargo.toml.orig in package source
                       (when (file-exists? "Cargo.toml.orig")
                         (delete-file "Cargo.toml.orig"))

                       ;; Use unstable feature ‘--registry’.
                       (setenv "RUSTC_BOOTSTRAP" "1")
                       (for-each
                        (lambda (pkg)
                          (invoke "cargo" "package" "--offline" "--package" pkg
                                  "--registry" "crates-io" "-Z" "package-workspace"
                                  "--no-metadata" "--no-verify")
                          (for-each
                           (lambda (crate)
                             (invoke "tar" "xzf" crate "-C" "guix-vendor"))
                           (begin
                             (delete-file-recursively "target/package/tmp-registry")
                             (find-files "target/package" "\\.crate$")))
                          ((assoc-ref %standard-phases 'patch-cargo-checksums)))
                        '("smithay" "smithay-drm-extras"))
                       (unsetenv "RUSTC_BOOTSTRAP")))))))
      (inputs rust-smithay-cargo-inputs)
      (home-page "https://github.com/Smithay/smithay")
      (synopsis "Smithy for Rust Wayland compositors")
      (description
       "Smithay aims to provide building blocks to create wayland compositors in
Rust.  While not being a full-blown compositor, it'll provide objects and
interfaces implementing common functionalities that pretty much any compositor
will need, in a generic fashion.

It supports the @code{wayland}, @code{wayland-protocols}, and some external
extensions, such as @code{wlr-protocols} and @code{plasma-wayland-protocols}.")
      (license license:expat))))

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
                    (("# version = \"0.4.1\"")
                     "version = \"0.4.0\"")
                    (("# version = \"0.1.0\"")
                     "version = \"0.1.0\"")
                    (("git.*optional")
                     "version = \"0.8.0\", optional")
                    (("^git = .*")
                     ""))))
              (add-after 'configure 'set-rust-flags
                (lambda _
                  (setenv "RUSTFLAGS" (string-join
                                       '("-C" "link-arg=-lEGL"
                                         "-C" "link-arg=-lwayland-client")
                                       " "))))
              (add-before 'check 'prepare-test-environment
                (lambda _
                  (setenv "XDG_RUNTIME_DIR" "/tmp")))
              (add-after 'install 'install-extras
                (lambda _
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
           libdisplay-info
           libinput-minimal
           libseat
           libxkbcommon
           mesa
           pango
           pipewire
           rust-pipewire
           rust-smithay
           wayland
           niri-cargo-inputs))
   (home-page "https://github.com/YaLTeR/niri")
   (synopsis "Scrollable-tiling Wayland compositor")
   (description
    "Niri is a scrollable-tiling Wayland compositor which arranges windows in a
scrollable format.  It is considered stable for daily use and performs most
functions expected of a Wayland compositor.")
   (license license:gpl3)))
