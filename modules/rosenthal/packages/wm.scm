;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2025 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal packages wm)
  ;; Utilities
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (rosenthal utils cargo)
  ;; Guix origin methods
  #:use-module (guix git-download)
  ;; Guix build systems
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system copy)
  ;; Guix packages
  #:use-module (gnu packages admin)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages calendar)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages hardware)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg))

(define-public noctalia-shell
  (package
    (name "noctalia-shell")
    (version "4.7.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/noctalia-dev/noctalia-shell")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0qylp37xrb71c3a5fr55a5zr7ibb4nd87jkdw01l54ij15jrdqxd"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("." "etc/xdg/quickshell/noctalia-shell"))
      #:imported-modules
      `((guix build qt-utils)
        ,@%copy-build-system-modules)
      #:modules
      '((srfi srfi-26)
        (guix build copy-build-system)
        (guix build qt-utils)
        (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-references
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "Services/Power/IdleInhibitorService.qml"
                (("systemd-inhibit")
                 (search-input-file inputs "bin/elogind-inhibit")))))
          (add-after 'unpack 'reduce-output-size
            (lambda _
              (delete-file-recursively "Assets/Screenshots")))
          (add-after 'install 'make-wrapper
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((script "noctalia-shell"))
                (with-output-to-file script
                  (lambda ()
                    (format #t "~
#!~a
exec ~a --config ~a/etc/xdg/quickshell/noctalia-shell \"$@\"~%"
                            (search-input-file inputs "bin/sh")
                            (search-input-file inputs "bin/quickshell")
                            #$output)))
                (wrap-script script
                  `("PATH"
                    suffix
                    ,(map (compose dirname
                                   (cut search-input-file inputs <>))
                          '("bin/bluetoothctl"
                            "bin/brightnessctl"
                            "bin/cliphist"
                            "bin/convert"
                            "bin/ddcutil"
                            "bin/fastfetch"
                            "bin/fc-list"
                            "bin/find"
                            "bin/getent"
                            "bin/git"
                            "bin/grep"
                            "bin/khal"
                            "bin/ls"
                            "bin/nmcli"
                            "bin/python3"
                            "bin/sh"
                            "bin/which"
                            "bin/wl-paste"
                            "bin/wlsunset"
                            "bin/wtype"))))
                (chmod script #o555)
                (install-file script (in-vicinity #$output "bin")))))
          (add-after 'make-wrapper 'qt-wrap
            (lambda args
              (apply wrap-all-qt-programs
                     #:qtbase #$(this-package-input "qtbase")
                     args))))))
    (inputs
     (list bash-minimal
           bluez
           brightnessctl
           cliphist
           coreutils-minimal
           ddcutil
           elogind
           fastfetch-minimal
           findutils
           fontconfig
           git-minimal
           glibc
           grep
           guile-3.0
           imagemagick
           khal
           network-manager
           noctalia-qs
           python-minimal
           qtbase
           qtmultimedia
           qtwayland
           which
           wl-clipboard
           wlsunset
           wtype))
    (home-page "https://noctalia.dev/")
    (synopsis "Wayland desktop shell")
    (description
     "Noctalia is a minimal desktop shell designed for Wayland, built on the
@code{quickshell} framework.  It offers a customizable and clean user interface,
supporting various Wayland compositors like @code{niri}, @code{hyprland}, and
@code{sway}.")
    (license license:expat)))

(define-public noctalia-qs
  (package
    (inherit quickshell)
    (name "noctalia-qs")
    (version "0.0.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/noctalia-dev/noctalia-qs")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pyakmaq2bwdhnnqnrsqm9g1mjf5spij6hsvmlw058kxj8xkbbd4"))))
    (inputs
     (modify-inputs (package-inputs quickshell)
       (prepend glib polkit)))
    (home-page "https://noctalia.dev/")
    (synopsis "QtQuick-based desktop shell toolkit (Noctalia fork)")))
