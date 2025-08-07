;;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages xorg)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (rosenthal utils cargo)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xorg))

(define-public xwayland-satellite
  (package
    (name "xwayland-satellite")
    (version "0.5.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Supreeeme/xwayland-satellite")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1r99qfbmc67202pcs4kiw94hiql0aqcsx877bgnlyxy6gzilq47y"))))
    (build-system cargo-build-system)
    (arguments
     (list #:install-source? #f
           #:tests? #f                  ;Requires running display server.
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-paths
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "src/lib.rs"
                     (("\"Xwayland\"")
                      (format #f "\"~a\""
                              (search-input-file inputs "bin/Xwayland")))))))))
    (native-inputs (list pkg-config))
    (inputs
     (cons* clang
            xcb-util-cursor
            xorg-server-xwayland
            (rosenthal-cargo-inputs 'xwayland-satellite)))
    (home-page "https://github.com/Supreeeme/xwayland-satellite")
    (synopsis "Xwayland outside your Wayland")
    (description
     "@command{xwayland-satellite} grants rootless Xwayland integration to any
Wayland compositor implementing @code{xdg_wm_base} interface.  This is
particularly useful for compositors that (understandably) do not want to go
through implementing support for rootless Xwayland themselves.")
    (license license:mpl2.0)
    (properties
    '((disable-updater? . #t)))))
