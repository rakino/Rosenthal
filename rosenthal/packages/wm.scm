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
         (replace "wayland-protocols" wayland-protocols-1.31)))
      (native-inputs (list `(,hwdata "pnp") pkg-config)))))

(define-public wlroots-dev
  (let ((base wlroots-0.16.0)
        (revision "169")
        (commit "86fc2199f85ac0e1089bb7fd5a0b5cbc432bdb67"))
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
                  "04cd03pxwpxwrfh6w0d34hqmi1kp9qn1yp6xbwdgvqzw61xbh7j1")))))))

(define-public hyprland-protocols
  (let ((revision "1")
        (commit "301733ae466b229066ba15a53e6d8b91c5dcef5b"))
    (package
      (name "hyprland-protocols")
      (version (git-version "0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/hyprwm/hyprland-protocols")
                      (commit commit)))
                (file-name (git-file-name name version))
                (modules '((guix build utils)))
                (snippet
                 '(substitute* "meson.build"
                    (("pkgconfig_configuration.set\\('PACKAGE', 'hyprland-protocols'\\)") "")))
                (sha256
                 (base32
                  "1q54lvpiilc75z19m2i0zc3pg0nxp4667741d5p8zlci4bjid9g0"))))
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
                        "8bd7234d7256d494794741f973f470458a1ad904" "/nix/"
                        "meson-build.patch"))
    (file-name "hyprland-unbundle-wlroots.patch")
    (sha256
     (base32 "1lh4gfm8x3pqfl2bkw2iy9b6ckgkhjfgnyyg45iyj0x2sh66lrng"))))

(define-public hyprland
  (package
    (name "hyprland")
    (version "0.20.0beta")
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
                "0hk2d2rc2hjr4f8aqpq8scqryrl518m1h3kqgcy142r1rwjmf286"))))
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
    (inputs (list hyprland-protocols pango-next pixman-0.42.2 wlroots-dev))
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
  (let ((revision "0")
        (commit "5b21c74a3200ffdd48ed7764c7041d43c3cd5588"))
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
                  "1wbpmf1dsf1syh7jbirglqkscsrn7l496j7afwxpbdjragagbhc0"))))
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

(define-public waylock
  (package
    (name "waylock")
    (version "0.4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/ifreund/waylock"
                                  "/releases/download/v" version
                                  "/waylock-" version".tar.gz"))
              (sha256
               (base32
                "0nvzsi2mbmjvnw475srr2iwqpy3p32sgfkdkm0hsr7c0i2v82f3i"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                  ;no tests
           #:make-flags
           #~(list "-Dcpu=baseline" "-Dpie" "-Drelease-safe")
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)
               (add-after 'unpack 'set-zig-env
                 (lambda _
                   (setenv "CC" #$(cc-for-target))
                   (setenv "XDG_CACHE_HOME" "no-thanks")))
               (replace 'build
                 (lambda* (#:key (make-flags '()) #:allow-other-keys)
                   (apply invoke "zig" "build" `(,@make-flags))))
               (replace 'install
                 (lambda _
                   (copy-recursively "zig-out" #$output))))))
    (native-inputs (list pkg-config scdoc zig))
    (inputs
     (list libxkbcommon-minimal
           linux-pam
           wayland-1.21.0
           wayland-protocols-1.31))
    (home-page "https://github.com/ifreund/waylock")
    (synopsis "Small screenlocker for Wayland compositors")
    (description
     "Waylock is a small screenlocker for Wayland compositors implementing
@code{ext-session-lock-v1}.  The @code{ext-session-lock-v1} protocol is
significantly more robust than previous client-side Wayland screen locking
approaches.  Importantly, the screenlocker crashing does not cause the session
to be unlocked.")
    (license license:isc)))
