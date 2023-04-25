;; SPDX-FileCopyrightText: 2022-2023 Hilton Chain <hako@ultrarare.space>
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
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xorg)
  #:use-module (rosenthal packages freedesktop)
  #:use-module (rosenthal packages xdisorg))

(define wlroots-for-hyprland
  (let ((base wlroots)
        (revision "212")
        (commit "5f264a7d6c8af27d41ff440c05262b022c055593"))
    (package
      (inherit base)
      (name "wlroots-for-hyprland")
      (version (git-version "0.16.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.freedesktop.org/wlroots/wlroots.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1qgdpigisgzvhf5m48v0vp21hn4fpljkjgij9wlmlmkv7rk2idj9"))))
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
         (append libdrm-2.4.115 libxcb xcb-util-renderutil)
         (replace "libxkbcommon" libxkbcommon-minimal)
         (replace "pixman" pixman-0.42.2)
         (replace "wayland" wayland-1.21.0)
         (replace "wayland-protocols" wayland-protocols-1.31)))
      (native-inputs (list `(,hwdata "pnp") pkg-config)))))

(define-public hyprland-protocols
  (let ((revision "2")
        (commit "eb7dcc0132ad25addc3e8d434c4bfae6bd3a8c90"))
    (package
      (name "hyprland-protocols")
      (version (git-version "0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/hyprwm/hyprland-protocols")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "17basmdwi47j1shv8v6wbwyzljipxczk52d0dhwgsz3z1x9f0hl2"))))
      (build-system meson-build-system)
      (home-page "https://hyprland.org")
      (synopsis "Wayland protocol extensions for Hyprland")
      (description
       "This package provides Wayland protocol extensions for Hyprland and it
exists in an effort to bridge the gap between Hyprland and KDE/Gnome's
functionality.  Since @code{wlr-protocols} is closed for new submissions, and
@code{wayland-protocols} is very slow with changes, this package will hold
protocols used by Hyprland to bridge the aforementioned gap.")
      (license license:bsd-3))))

(define hyprland-unbundle-wlroots-patch
  (origin
    (method url-fetch)
    (uri (string-append "https://github.com/hyprwm/Hyprland" "/raw/"
                        "2f6b37e1032fbacd506978e0098807e1b334aa5d" "/nix/"
                        "meson-build.patch"))
    (file-name "hyprland-unbundle-wlroots.patch")
    (sha256
     (base32 "083r6a2pmnnrz1jz3g3ikx8dffs3ajdd49gawnh3skm59snanr2l"))))

(define-public hyprland
  (package
    (name "hyprland")
    (version "0.21.0beta")
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
                "0ff4lpg82bmxzlx8kb11shb550fqq8arp5gycg702qc9yjzsqnqi"))))
    (build-system meson-build-system)
    (arguments
     (list #:build-type "release"
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-default-wallpaper-path
                 (lambda _
                   (substitute* "src/render/OpenGL.cpp"
                     (("/usr") #$output)))))))
    (native-inputs (list gcc-12 jq pkg-config))
    (inputs (list cairo hyprland-protocols pixman-0.42.2 wlroots-for-hyprland))
    (home-page "https://hyprland.org")
    (synopsis "Dynamic tiling Wayland compositor based on wlroots")
    (description
     "@code{Hyprland} is a dynamic tiling Wayland compositor based on
@code{wlroots} that doesn't sacrifice on its looks.  It supports multiple
layouts, fancy effects, has a very flexible IPC model allowing for a lot of
customization, and more.")
    (license license:bsd-3)))

;; No releases yet.
(define-public grimblast
  (let ((revision "1")
        (commit "37c8121f98d76f57caa00dd7106877876e0d7483"))
    (package
      (name "grimblast")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/hyprwm/contrib")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1jhjjblklqnz601ic58bl0x550d1rkyaqdmydhqzx3k2bc2mnjk7"))))
      (build-system gnu-build-system)
      (arguments
       (list #:tests? #f                ;no tests
             #:make-flags
             #~(list (string-append "PREFIX=" #$output))
             #:phases
             #~(modify-phases %standard-phases
                 (delete 'configure)
                 (add-after 'unpack 'chdir
                   (lambda _
                     (chdir "grimblast")))
                 (add-after 'chdir 'patch-script-dependencies
                   (lambda* (#:key inputs #:allow-other-keys)
                     (substitute* "grimblast"
                       (("\\b(date|grim|jq|notify-send|slurp|hyprctl|wl-copy)\\b"
                         _ binary)
                        (search-input-file
                         inputs (string-append "bin/" binary)))))))))
      (native-inputs (list scdoc))
      (inputs (list grim jq libnotify slurp hyprland wl-clipboard))
      (home-page "https://github.com/hyprwm/contrib")
      (synopsis "Hyprland version of Grimshot")
      (description "A Hyprland version of Grimshot.")
      (license license:expat))))
