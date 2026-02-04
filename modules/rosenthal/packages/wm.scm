;;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages wm)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system copy)
  #:use-module (rosenthal utils cargo)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg))

(define-public noctalia-shell
  (package
    (name "noctalia-shell")
    (version "4.3.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/noctalia-dev/noctalia-shell")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1rv41ph1159wg1nzb8qn6lg6s0ycg7zl7qkaaqxwd4ld2i3g8km5"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("." "share/noctalia-shell"))
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
          (add-after 'install 'make-wrapper
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((script "noctalia-shell"))
                (with-output-to-file script
                  (lambda ()
                    (format #t "~
#!~a
exec ~a --path ~a/share/noctalia-shell \"$@\"~%"
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
                            "bin/cava"
                            "bin/cliphist"
                            "bin/convert"
                            "bin/fastfetch"
                            "bin/fc-list"
                            "bin/find"
                            "bin/git"
                            "bin/ls"
                            "bin/nmcli"
                            "bin/python3"
                            "bin/sh"
                            "bin/which"
                            "bin/wlsunset"))))
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
           cava
           cliphist
           coreutils-minimal
           fastfetch
           findutils
           fontconfig
           git-minimal
           guile-3.0
           imagemagick
           network-manager
           python-minimal
           qtbase
           qtwayland
           quickshell
           which
           wlsunset))
    (home-page "https://noctalia.dev/")
    (synopsis "Wayland desktop shell")
    (description
     "Noctalia is a minimal desktop shell designed for Wayland, built on the
@code{quickshell} framework.  It offers a customizable and clean user interface,
supporting various Wayland compositors like @code{niri}, @code{hyprland}, and
@code{sway}.")
    (license license:expat)))
