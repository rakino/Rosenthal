;; SPDX-FileCopyrightText: 2022 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages freedesktop)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages check)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages python))

;; https://issues.guix.gnu.org/63847
(define-public libinput-minimal-1.23.0
  (package
    (inherit libinput-minimal)
    (name "libinput-minimal")
    (version "1.23.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.freedesktop.org/libinput/libinput.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0wnqfnxxvf9jclh64hrb0scn3s1dmwdkmqf7hp0cfmjz5n5nnv7d"))))
    (native-inputs
     (modify-inputs (package-native-inputs libinput)
       (append python-minimal-wrapper python-pytest)))))

(define-public wayland-protocols-1.31
  (let ((base wayland-protocols))
    (package
      (inherit base)
      (name "wayland-protocols")
      (version "1.31")
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://gitlab.freedesktop.org/wayland/wayland-protocols"
                      "/-/releases/" version "/downloads/"
                      "wayland-protocols-" version ".tar.xz"))
                (sha256
                 (base32
                  "0f72359fzvh6jzri4fd79m34rwm2r55p2ryq4306wrw7xliafzx0")))))))

(define-public xdg-desktop-portal-hyprland
  (package
    (name "xdg-desktop-portal-hyprland")
    (version "0.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/hyprwm/xdg-desktop-portal-hyprland")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
                (base32
                  "1dcglmx02j73qbmw9qsacamn8byakyzwknpqjnhsyphixb4crrdg"))))
    (build-system meson-build-system)
    (arguments
      (list
        #:configure-flags #~(list "-Dsystemd=disabled")))
    (native-inputs (list cmake pkg-config wayland hyprland-protocols))
    (inputs (list elogind hyprland-protocols pipewire wayland-protocols wayland libinih `(,util-linux "lib")))
    (home-page "https://github.com/hyprwm/xdg-desktop-portal-hyprland")
    (synopsis "@code{xdg-desktop-portal} backend for Hyprland")
    (description
      "This package provides @code{xdg-desktop-portal-hyprland}. This project extends @code{xdg-desktop-portal-wlr} for Hyprland, adding support for @code{xdg-desktop-portal} screenshot and casting interfaces while adding a few extra portals specific to Hyprland: mostly for window sharing. Requires @code{hyprland-share-picker}.")
    (license license:expat)))

(define-public hyprland-share-picker
  (package
    (inherit xdg-desktop-portal-hyprland)
    (name "hyprland-share-picker")
    (build-system qt-build-system)
    (inputs (modify-inputs (package-inputs xdg-desktop-portal-hyprland)
                           (append qtwayland-5 slurp)))
    (native-inputs (modify-inputs (package-native-inputs xdg-desktop-portal-hyprland)
                                  (append qtwayland-5)))
    (arguments
      (list
        #:tests? #f
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'chdir
              (lambda _ (chdir "hyprland-share-picker"))))))
    (synopsis "Helper program for @code{xdg-desktop-portal-hyprland}.")))
