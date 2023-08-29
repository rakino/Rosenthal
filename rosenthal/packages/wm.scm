;; SPDX-FileCopyrightText: 2022-2023 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages wm)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system qt)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
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
  #:use-module (gnu packages xorg)
  #:use-module (rosenthal packages freedesktop)
  #:use-module (rosenthal packages xdisorg))

(define hwdata-for-hyprland
  (package
    (inherit hwdata)
    (arguments
     (substitute-keyword-arguments (package-arguments hwdata)
       ((#:phases _) #~%standard-phases)))
    (outputs '("out"))))

(define libdisplay-info-for-hyprland
  (package
    (name "libdisplay-info")
    (version "0.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.freedesktop.org/emersion/libdisplay-info")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ffq7w1ig1y44rrmkv1hvfjylzgq7f9nlnnsdgdv7pmcpfh45pgf"))))
    (build-system meson-build-system)
    (arguments '(#:tests? #f))          ;TODO
    (native-inputs (list hwdata-for-hyprland pkg-config python-minimal-wrapper))
    (home-page "https://emersion.pages.freedesktop.org/libdisplay-info/")
    (synopsis "EDID and DisplayID library")
    (description "This package provides an EDID and DisplayID library.")
    (license license:expat)))

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
        (revision "724")
        (commit "717ded9bb0191ea31bf4368be32e7a15fe1b8294"))
    (package
      (inherit base)
      (name "wlroots")
      (version (git-version "0.16.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.freedesktop.org/wlroots/wlroots.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0k4xhdpcv6c38vb2m3khjbikcmj48z3ry1696rv0zijcpcds84kq"))))
      (propagated-inputs
       (modify-inputs (package-propagated-inputs base)
         (append libdisplay-info-for-hyprland libxcb xcb-util-renderutil)
         (replace "libinput-minimal" libinput-minimal-1.23.0)
         (replace "pixman" pixman-0.42.2)))
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

(define hyprland-unbundle-wlroots-patch
  (origin
    (method url-fetch)
    (uri (string-append "https://github.com/hyprwm/Hyprland" "/raw/"
                        "72d2f33b34951b7f5be6cdbc302b1c6a33cbf60f" "/nix/"
                        "meson-build.patch"))
    (file-name "hyprland-unbundle-wlroots.patch")
    (sha256
     (base32 "1wj0kwvkkk2r7k18m9i2hdp9i9z7n330dib27jlbc8mjr96976y5"))))

(define-public hyprland
  (package
    (name "hyprland")
    (version "0.29.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/hyprwm/Hyprland"
                                  "/releases/download/v" version
                                  "/source-v" version ".tar.gz"))
              (modules '((guix build utils)))
              (snippet '(delete-file-recursively "subprojects"))
              (patches (list hyprland-unbundle-wlroots-patch))
              (sha256
               (base32
                "0w4lx8ixdv4pi67kavs3r3d4zykmhn4pigch6dwqidgxsf382j9a"))))
    (build-system meson-build-system)
    (arguments
     (list #:build-type "release"
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-path
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "src/render/OpenGL.cpp"
                     (("/usr") #$output))
                   (substitute* (find-files "src" "\\.cpp")
                     (("(execAndGet\\(\\(?\")\\<(cat|fc-list|lspci|nm)\\>"
                       _ pre cmd)
                      (format #f "~a~a"
                              pre
                              (search-input-file
                               inputs (string-append "/bin/" cmd)))))))
               (add-after 'unpack 'substitute-meson-build
                 (lambda _
                   (substitute* "meson.build"
                     (("\\<git\\>") "true")
                     ((".*@.*") "")))))))
    (native-inputs (list gcc-12 jq pkg-config))
    (inputs
     (list hyprland-protocols
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
    (version "0.5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hyprwm/xdg-desktop-portal-hyprland")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1dmgc0w4wjj9hwqg17wg529v8sbxr6czp9s319d5407jm780x40b"))))
    (build-system meson-build-system)
    (arguments
     (list #:imported-modules
           (append %meson-build-system-modules
                   %qt-build-system-modules)
           #:modules
           '((guix build utils)
             (guix build meson-build-system)
             ((guix build qt-build-system) #:prefix qt:))
           #:tests? #f                  ;No tests
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-path
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* (find-files "." "\\.cp?*$")
                     (("/bin/sh") "sh")
                     (("\\<(sh|grim|slurp)\\>" _ cmd)
                      (search-input-file inputs (string-append "/bin/" cmd)))
                     (("\"(hyprland-share-picker)\"" _ cmd)
                      (string-append "\"" #$output "/bin/" cmd "\"")))))
               ;; After building the portal, we need to build the share selector
               ;; using qt
               (add-after 'install 'chdir
                 (lambda _
                   (chdir "../source/hyprland-share-picker/")))
               (add-after 'chdir 'qt-build
                 (lambda* (#:key (make-flags '()) (parallel-build? #t)
                           #:allow-other-keys)
                   ((assoc-ref qt:%standard-phases 'build)
                    #:make-flags make-flags
                    #:parallel-build? parallel-build?)))
               (add-after 'qt-build 'install-hyprland-share-picker
                 (lambda _
                   (install-file "build/hyprland-share-picker"
                                 (string-append #$output "/bin"))))
               (add-after 'install-hyprland-share-picker 'qt-wrap
                 (assoc-ref qt:%standard-phases 'qt-wrap)))))
    (native-inputs (list cmake-minimal pkg-config))
    (inputs
     (list bash-minimal
           basu
           grim
           hyprland-protocols
           libinih
           mesa
           pipewire
           qtbase-5
           slurp
           `(,util-linux "lib")
           wayland
           wayland-protocols))
    (home-page "https://github.com/hyprwm/xdg-desktop-portal-hyprland")
    (synopsis "XDG Desktop Portal backend for Hyprland")
    (description
     "This package provides @code{xdg-desktop-portal-hyprland}, which extends
@code{xdg-desktop-portal-wlr} for Hyprland with support for
@code{xdg-desktop-portal} screenshot and casting interfaces, while adding a few
extra portals specific to Hyprland, mostly for window sharing.")
    (license license:expat)))
