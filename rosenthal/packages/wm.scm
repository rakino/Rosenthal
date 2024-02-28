;; SPDX-FileCopyrightText: 2022-2023 Hilton Chain <hako@ultrarare.space>
;; SPDX-FileCopyrightText: 2024 Apathetic Wendigo <coleclough.lucy@gmail.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages wm)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages cmake)
  #:use-module (rosenthal packages freedesktop))

(define cairo-for-hyprland
  (package
    (inherit cairo)
    (name "cairo")
    (version "1.18.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://cairographics.org/releases/cairo-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0r0by563s75xyzz0d0j1nmjqmdrk2x9agk7r57p3v8vqp4v0ffi4"))))
    (build-system meson-build-system)
    (arguments
     (list #:tests? #f
           #:glib-or-gtk? #t
           #:configure-flags
           #~(list "-Dspectre=disabled")))
    (outputs '("out"))))

(define hwdata-for-hyprland
  (package
    (inherit hwdata)
    (arguments
     (substitute-keyword-arguments (package-arguments hwdata)
       ((#:phases _) #~%standard-phases)))
    (outputs '("out"))))

(define libdrm-for-hyprland
  (package
    (inherit libdrm)
    (name "libdrm")
    (version "2.4.120")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://dri.freedesktop.org/libdrm/libdrm-"
                    version ".tar.xz"))
              (sha256
               (base32
                "0yijzgg6rdsa68bz03sw0lcfa2nclv9m3as1cja50wkcyxim7x9v"))))))

(define udis86-for-hyprland
  (let ((revision "186")
        (commit "5336633af70f3917760a6d441ff02d93477b0c86"))
    (package
      (name "udis86")
      (version (git-version "1.7.2" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/canihavesomecoffee/udis86")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0y5z1169wff578jylpafsww4px4y6gickhcs885a9c660d8xs9qy"))))
      (build-system gnu-build-system)
      (native-inputs (list autoconf automake libtool python-minimal-wrapper))
      (home-page "https://github.com/canihavesomecoffee/udis86")
      (synopsis "Disassembler Library for x86 and x86-64")
      (description
       "Udis86 is a disassembler for the x86 and x86-64 class of instruction
set architectures.  It consists of a C library called @code{libudis86} and a
command line tool called @code{udcli} that incorporates the library.")
      (license license:bsd-2))))

(define wlroots-for-hyprland
  (let ((base wlroots)
        (revision "168")
        (commit "00b869c1a96f300a8f25da95d624524895e0ddf2"))
    (package
      (inherit base)
      (name "wlroots")
      (version (git-version "0.17.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.freedesktop.org/wlroots/wlroots.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "18s8iw3w8g3qwd6kw03r2myms5hxhl4xryy9vmpj573y98di6xg4"))))
      (propagated-inputs
       (modify-inputs (package-propagated-inputs wlroots)
         (prepend libdisplay-info libdrm-for-hyprland)
         (replace "libinput-minimal" libinput-minimal-1.24.0)))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "hwdata" `(,hwdata-for-hyprland "out")))))))

(define-public hyprland-protocols
  (package
    (name "hyprland-protocols")
    (version "0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hyprwm/hyprland-protocols")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1drjznj7fn6m5m6skhzh0p031cb5x0bb4i56jxnxwpwaa71g1z20"))))
    (build-system meson-build-system)
    (home-page "https://hyprland.org")
    (synopsis "Wayland protocol extensions for Hyprland")
    (description
     "This package provides Wayland protocol extensions for Hyprland and it
exists in an effort to bridge the gap between Hyprland and KDE/Gnome's
functionality.  Since @code{wlr-protocols} is closed for new submissions, and
@code{wayland-protocols} is very slow with changes, this package will hold
protocols used by Hyprland to bridge the aforementioned gap.")
    (license license:bsd-3)))

(define tomlplusplus
  (package
    (name "tomlplusplus")
    (version "3.4.0")
    (source (origin
        (method url-fetch)
        (uri (string-append "https://github.com/marzer/tomlplusplus"
                            "/archive/refs/tags/v"
                            version
                            ".tar.gz"))
        (sha256
         (base32 "0maisabrwnz1fad5va7gnpgwmh9q39ikdfwfryfaxym471czc5w5"))))
    (build-system meson-build-system)
    (native-inputs (list cmake))
    (home-page "https://marzer.github.io/tomlplusplus/")
    (synopsis "TOML config parser and serializer for C++")
    (description
     "Header-only TOML config file parser and serializer for C++17")
    (license license:expat)))

(define hyprland-unbundle-wlroots-patch
  (origin
    (method url-fetch)
    (uri (string-append "https://github.com/hyprwm/Hyprland" "/raw/"
                        "d70cc88dab11bc6d1095523a0ce655dff40b27a2"
                        "/nix/patches/meson-build.patch"))
    (sha256
     (base32 "1rxv5354561hp625pq901fj77r1qkayfldh13w5vq182s8scj44l"))))

(define-public hyprland
  (package
    (name "hyprland")
    (version "0.35.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/hyprwm/Hyprland"
                                  "/releases/download/v" version
                                  "/source-v" version ".tar.gz"))
              (modules '((guix build utils)))
              (snippet
               ;; Remove bundled sources
               '(delete-file-recursively "subprojects"))
              (patches (list hyprland-unbundle-wlroots-patch))
              (sha256
               (base32
                "06wcpcb3hd6av3scv9qnvp6zs6g6sqx8m7r0rjmchcb1c1jdflyy"))))
    (build-system meson-build-system)
    (arguments
     (list #:build-type "release"
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-path
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* (find-files "src" "\\.cpp$")
                     (("/usr/local(/bin/Hyprland)" _ path)
                      (string-append #$output path))
                     (("/usr") #$output)
                     (("(execAndGet\\(\")\\<(cat|fc-list|lspci)\\>"
                       _ pre cmd)
                      (string-append
                       pre (search-input-file
                            inputs (string-append "bin/" cmd))))
                     (("\\<cc\\>") (search-input-file inputs "bin/gcc"))
                     ;; NOTE: Add binutils to inputs will override ld-wrapper.
                     (("(execAndGet\\(\\(\")\\<nm\\>" _ pre)
                      (string-append pre #$binutils "/bin/nm"))
                     (("\\<objcopy\\>")
                      (string-append #$binutils "/bin/objcopy"))))))))
    (native-inputs
     (list gcc-13
           jq
           pkg-config
           tomlplusplus))
    (inputs
     (list cairo-for-hyprland
           gcc-13
           hyprland-protocols
           pango
           pciutils
           udis86-for-hyprland
           wlroots-for-hyprland))
    (home-page "https://hyprland.org")
    (synopsis "Dynamic tiling Wayland compositor based on wlroots")
    (description
     "Hyprland is a dynamic tiling Wayland compositor based on @code{wlroots}
that doesn't sacrifice on its looks.  It supports multiple layouts, fancy
effects, has a very flexible IPC model allowing for a lot of customization, and
more.")
    (license license:bsd-3)))

(define-public hyprlang
  (package
    (name "hyprlang")
    (version "0.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hyprwm/hyprlang")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "12lxqza2crbdszvkgd1sz0rwjf80i835r77k9q69fi3x32pdjvcx"))))
    (build-system cmake-build-system)
    (native-inputs (list gcc-13))
    (home-page "https://hyprland.org/hyprlang/")
    (synopsis "Official implementation library for hypr config language")
    (description
     "This package provides the official implementation for hypr configuration
language used in @code{hyprland}.")
    (license license:gpl3+)))

(define-public grimblast
  (package
    (name "grimblast")
    (version "0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hyprwm/contrib")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ld0sj7ahf9jf8cqzbqkhj3m2w60027ixic24ih26nwy90b5qjwx"))))
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
               (add-after 'install 'wrap
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let ((grimblast (string-append #$output "/bin/grimblast")))
                     (wrap-script grimblast
                       `("PATH" suffix
                         ,(map (lambda (program)
                                 (dirname (search-input-file
                                           inputs (string-append "/bin/" program))))
                               '("grim" "slurp" "hyprctl" "wl-copy" "jq"
                                 "notify-send" "date"))))))))))
    (native-inputs (list scdoc))
    (inputs (list grim guile-3.0 jq libnotify slurp hyprland wl-clipboard))
    (home-page "https://github.com/hyprwm/contrib")
    (synopsis "Hyprland version of Grimshot")
    (description "A Hyprland version of Grimshot.")
    (license license:expat)))

(define-public xdg-desktop-portal-hyprland
  (package
    (name "xdg-desktop-portal-hyprland")
    (version "1.3.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hyprwm/xdg-desktop-portal-hyprland")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0fdbzxanmmzrvb9wfzg1pdsnlg7dl6v5k8bl44w10n48s7bbbzn0"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f                  ;No tests
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-path
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* (find-files "." "\\.cp?*$")
                     (("/bin/sh") "sh")
                     (("\\<(sh|grim|hyprctl|slurp)\\>" _ cmd)
                      (search-input-file inputs (string-append "/bin/" cmd)))
                     (("\\<(hyprland-share-picker)\\>" _ cmd)
                      (string-append #$output "/bin/" cmd))))))))
    (native-inputs
     (list gcc-13 pkg-config wayland))
    (inputs
     (list bash-minimal
           grim
           hyprland
           hyprland-protocols
           hyprlang
           mesa
           pipewire
           qtbase-5
           sdbus-c++
           slurp
           wayland-protocols))
    (home-page "https://github.com/hyprwm/xdg-desktop-portal-hyprland")
    (synopsis "XDG Desktop Portal backend for Hyprland")
    (description
     "This package provides @code{xdg-desktop-portal-hyprland}, which extends
@code{xdg-desktop-portal-wlr} for Hyprland with support for
@code{xdg-desktop-portal} screenshot and casting interfaces, while adding a few
extra portals specific to Hyprland, mostly for window sharing.")
    (license license:bsd-3)))
