;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages rust-apps)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
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
  #:use-module (rosenthal packages)
  #:use-module (rosenthal packages rust-crates))

(define-public atuin
  (package
    (name "atuin")
    (version "18.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/atuinsh/atuin")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1zi7ar999ycvig9c9crylab540xdgr0h6v99q9j8ypk9i1fviyiz"))
              (patches
               (rosenthal-patches "atuin-disable-failing-tests.patch"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:tests? #f                  ;TODO
      #:features
      ''("client" "sync" "server" "clipboard" "daemon")
      #:phases
      #~(modify-phases %standard-phases
          (replace 'install
            (lambda* (#:key outputs features #:allow-other-keys)
              (let* ((out      (assoc-ref outputs "out"))
                     (registry (string-append out "/share/cargo/registry"))
                     (sources  (string-append out "/share/cargo/src")))
                (mkdir-p out)
                ;; Make cargo reuse all the artifacts we just built instead
                ;; of defaulting to making a new temp directory
                (setenv "CARGO_TARGET_DIR" "./target")
                ;; Only install crates which include binary targets,
                ;; otherwise cargo will raise an error.
                (invoke "cargo" "install" "--no-track" "--path" "crates/atuin"
                        "--root" out "--features" (string-join features))))))))
    (inputs (force atuin-cargo-inputs))
    (home-page "https://atuin.sh/")
    (synopsis "Sync, search and backup shell history")
    (description
     "Atuin replaces existing shell history with a SQLite database, and records
additional context for commands.  Additionally, it provides optional and fully
encrypted synchronisation of history between machines, via an Atuin server.")
    (license license:gpl3)))

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
               "0vzskaalcz6pcml687n54adjddzgf5r07gggc4fhfsa08h1wfd4r"))
             (patches (list (local-file "./patches/niri.patch")))))
   (build-system cargo-build-system)
   (arguments
    (list #:install-source? #f
          #:phases
          #~(modify-phases %standard-phases
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
           wayland
           (force niri-cargo-inputs)))
   (home-page "https://github.com/YaLTeR/niri")
   (synopsis "Scrollable-tiling Wayland compositor")
   (description
    "Niri is a scrollable-tiling Wayland compositor which arranges windows in a
scrollable format.  It is considered stable for daily use and performs most
functions expected of a Wayland compositor.")
   (license license:gpl3)))
