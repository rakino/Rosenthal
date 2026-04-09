;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2022 Cairn <cairn@pm.me>
;;; Copyright © 2025 Carmine Margiotta <accounts@cmargiotta.net>
;;; Copyright © 2025 Noah Evans <noah@nevans.me>
;;; Copyright © 2026 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal packages games)
  ;; Guile builtins
  #:use-module (srfi srfi-26)
  ;; Utilities
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  ;; Guix origin methods
  #:use-module (guix git-download)
  ;; Guix build systems
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system qt)
  ;; Guix packages
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages java)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages markup)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages speech)
  #:use-module (gnu packages vulkan)
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
                    (commit version)
                    (recursive? #t)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0phmnc0fhzcw7dw35ldx3r387wxa3sjbcvq5yzy9dh9myg1ik6c6"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:imported-modules
      (append %cmake-build-system-modules
              %qt-build-system-modules)
      #:modules
      '((guix build utils)
        (guix build cmake-build-system)
        ((guix build qt-build-system) #:prefix qt:))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'qt-wrap
            (lambda args
              (apply (assoc-ref qt:%standard-phases 'qt-wrap)
                     #:qtbase #$(this-package-input "qtbase")
                     args)))
          (add-after 'qt-wrap 'wrap-program
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (wrap-program (search-input-file outputs "bin/prismlauncher")
                `("PATH" ":" prefix
                  ,(map (lambda (bin)
                          (dirname (search-input-file
                                    inputs (in-vicinity "bin" bin))))
                        '("glxinfo"
                          "lspci"
                          "xrandr")))
                `("LD_LIBRARY_PATH" ":" prefix
                  ,(map (lambda (lib)
                          (dirname (search-input-file
                                    inputs (in-vicinity "lib" lib))))
                        '("libflite.so"
                          "libglfw.so"
                          "libopenal.so"
                          "libudev.so"
                          "libusb-1.0.so"
                          "libvulkan.so"
                          ;; glfw
                          "libGL.so"
                          "libX11.so"
                          "libXcursor.so"
                          "libXext.so"
                          "libXrandr.so"
                          "libXxf86vm.so"
                          ;; openal
                          "libasound.so"
                          "libjack.so"
                          "libpipewire-0.3.so"
                          "libpulse.so")))))))))
    (native-inputs
     (list extra-cmake-modules
           pkg-config))
    (inputs
     (list alsa-lib
           bash-minimal
           cmark
           eudev
           flite
           gamemode
           glfw-3.4
           `(,icedtea-8 "jdk")
           jack-2
           libarchive
           libusb
           libx11
           libxcursor
           libxext
           libxrandr
           libxxf86vm
           mesa
           mesa-utils
           openal
           pciutils
           pipewire
           pulseaudio
           qrencode
           qtbase
           qtimageformats
           qtnetworkauth
           qtsvg
           qtwayland
           tomlplusplus
           vulkan-loader
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
