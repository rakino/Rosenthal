;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2022 Cairn <cairn@pm.me>
;;; Copyright © 2025 Carmine Margiotta <accounts@cmargiotta.net>
;;; Copyright © 2025 Noah Evans <noah@nevans.me>

(define-module (rosenthal packages games)
  ;; Utilities
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  ;; Guix origin methods
  #:use-module (guix git-download)
  ;; Guix build systems
  #:use-module (guix build-system cmake)
  ;; Guix packages
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages java)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages markup)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages xorg))

;; Copied from Guix Gaming Channels
(define-public prismlauncher/dolly
  (package
    (name "prismlauncher-dolly")
    (version "10.0.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/PrismLauncher/PrismLauncher")
                    (recursive? #t)
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0phmnc0fhzcw7dw35ldx3r387wxa3sjbcvq5yzy9dh9myg1ik6c6"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'patch-paths
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out            (assoc-ref outputs "out"))
                    (bin            (string-append out "/bin/prismlauncher"))
                    (xrandr         (assoc-ref inputs "xrandr"))
                    (qtwayland      (assoc-ref inputs "qtwayland"))
                    (qtsvg          (assoc-ref inputs "qtsvg")))
               (wrap-program bin
                 `("PATH" ":" prefix (,(string-append xrandr "/bin")))
                 `("QT_PLUGIN_PATH" ":" prefix ,(map (lambda (package)
                                                       (string-append package "/lib/qt6/plugins"))
                                                     (list qtwayland qtsvg)))
                 `("LD_LIBRARY_PATH" ":" prefix
                   (,@(map (lambda (dep)
                             (string-append (assoc-ref inputs dep)
                                            "/lib"))
                           '("libx11" "libxext" "libxcursor"
                             "libxrandr" "libxxf86vm" "pulseaudio" "mesa")))))))))))
    (native-inputs
     (list extra-cmake-modules
           pkg-config))
    (inputs
     (list bash-minimal ; for wrap-program
           cmark
           gamemode
           libarchive
           libx11
           libxcursor
           libxext
           libxrandr
           libxxf86vm
           mesa
           `(,openjdk17 "jdk")
           pulseaudio
           qrencode
           qt5compat
           qtbase
           qtnetworkauth
           qtsvg
           qtwayland
           tomlplusplus
           xrandr
           zlib))
    (home-page "https://prismlauncher.org/")
    (synopsis "Free, open source launcher for Minecraft")
    (description
     "Allows you to have multiple, separate instances of Minecraft (each with
their own mods, texture packs, saves, etc), and helps you manage them and
their associated options with a simple interface.")
    (license (list license:gpl3          ; PolyMC, launcher
                   license:expat         ; MinGW runtime, lionshead, tomlc99
                   license:lgpl3         ; Qt 5/6
                   license:lgpl3+        ; libnbt++
                   license:lgpl2.1+      ; rainbow (KGuiAddons)
                   license:isc           ; Hoedown
                   license:silofl1.1     ; Material Design Icons
                   license:lgpl2.1       ; Quazip
                   license:public-domain ; xz-minidec, murmur2, xz-embedded
                   license:bsd-3         ; ColumnResizer, O2 (Katabasis fork),
                                         ; gamemode, localpeer
                   license:asl2.0        ; classparser, systeminfo
                   ))))
