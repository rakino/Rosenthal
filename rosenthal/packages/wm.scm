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

(define hyprland-unbundle-wlroots-patch
  (origin
    (method url-fetch)
    (uri (string-append "https://github.com/hyprwm/Hyprland" "/raw/"
                        "8bd7234d7256d494794741f973f470458a1ad904" "/nix/"
                        "meson-build.patch"))
    (file-name "hyprland-unbundle-wlroots.patch")
    (sha256
     (base32 "1lh4gfm8x3pqfl2bkw2iy9b6ckgkhjfgnyyg45iyj0x2sh66lrng"))))

(define-public hyprland
  (package
    (name "hyprland")
    (version "0.18.0beta")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hyprwm/Hyprland")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (patches (list hyprland-unbundle-wlroots-patch))
              (modules '((guix build utils)))
              (snippet '(substitute* "meson.build"
                          (("git") "true")))
              (sha256
               (base32
                "1sbf12mqvgqpjfw4bni4p2gk2djnh49fwzki4dnsyq4ykcp7x6hb"))))
    (build-system meson-build-system)
    (arguments
     (list #:build-type "release"
           #:configure-flags
           #~(list "-Dxwayland=disabled")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-default-wallpaper-path
                 (lambda _
                   (substitute* "src/render/OpenGL.cpp"
                     (("/usr") #$output)))))))
    (native-inputs (list gcc-12 pkg-config))
    (inputs (list pango-next pixman-0.42.2 wlroots-0.16.0))
    (home-page "https://hyprland.org")
    (synopsis "Dynamic tiling Wayland compositor based on wlroots")
    (description
     "@code{Hyprland} is a dynamic tiling Wayland compositor based on
@code{wlroots} that doesn't sacrifice on its looks.  It supports multiple
layouts, fancy effects, has a very flexible IPC model allowing for a lot of
customization, and more.")
    (license license:bsd-3)))
