;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2022, 2025 Hilton Chain <hako@ultrarare.space>
;;; Copyright © 2025 William Goodspeed

(define-module (rosenthal packages admin)
  ;; Utilities
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (rosenthal utils cargo)
  ;; Guix origin methods
  #:use-module (guix git-download)
  ;; Guix build systems
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  ;; Guix packages
  #:use-module (gnu packages admin)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages pkg-config))

(define-public dinit
  (package
    (name "dinit")
    (version "0.20.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/davmac314/dinit")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bs61i08fnlmpkbz189ks1ljj0569avb51b22fk27mm99msmdjgy"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list "--shutdown-prefix=dinit-"
                   (string-append "--sbindir=" #$output "/sbin"))
           #:make-flags
           #~(list (string-append "CXX=" #$(cxx-for-target))
                   "CXX_FOR_BUILD=g++")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-paths
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "src/shutdown.cc"
                     (("(/sbin/swapoff|/bin/umount)" path)
                      (search-input-file inputs path))))))))
    (native-inputs (list m4))
    (inputs (list util-linux))
    (home-page "https://davmac.org/projects/dinit/")
    (synopsis "Service manager with dependency management")
    (description
     "Dinit is a service manager for Unix-like operating systems that allows
the user to manage services with dependencies and parallel startup.")
    (license license:asl2.0)))

(define-public libseat-sans-logind
  (package
    (inherit libseat)
    (name "libseat-sans-logind")
    (arguments
     (substitute-keyword-arguments arguments
       ((#:configure-flags configure-flags)
        #~(append #$configure-flags
                  (list "-Dlibseat-logind=disabled")))))
    (propagated-inputs '())
    (properties '((disable-updater? . #t)))))

(define-public pam-dumb-runtime-dir
  (package
    (name "pam-dumb-runtime-dir")
    (version "1.0.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ifreund/dumb_runtime_dir")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0nrxhvbh3bs4pi4f5h03zw1p1ys19qmmlx263ysly8302wkxk1m4"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                  ;No tests.
           #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target))
                   (string-append "DESTDIR=" #$output)
                   "PREFIX=")
           #:phases
           #~(modify-phases %standard-phases
               ;; No configure script.
               (delete 'configure))))
    (native-inputs (list pkg-config))
    (inputs (list linux-pam))
    (home-page "https://github.com/ifreund/dumb_runtime_dir")
    (synopsis "Create @code{XDG_RUNTIME_DIR} on login and never remove it")
    (description
     "This package creates an @code{XDG_RUNTIME_DIR} directory on login per
the freedesktop.org base directory spec.  Flaunts the spec and never removes
it, even after last logout. This keeps things simple and predictable.

The user is responsible for ensuring that the @file{/run/user} directory
exists and is only writable by root.")
    (license license:bsd-0)))

(define-public seatd-sans-logind
  (package
    (inherit seatd)
    (name "seatd-sans-logind")
    (arguments
     (substitute-keyword-arguments arguments
       ((#:configure-flags configure-flags)
        #~(append #$configure-flags
                  (list "-Dlibseat-logind=disabled")))))
    (propagated-inputs '())
    (properties '((disable-updater? . #t)))))
