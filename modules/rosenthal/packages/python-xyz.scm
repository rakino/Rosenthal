;;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages python-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages check)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages serialization)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (rosenthal utils cargo))

(define-public python-cryptg
  (package
    (name "python-cryptg")
    (version "0.5.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/cher-nov/cryptg")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0l2i8s96xdzq16svcfsvyd7chl9k6f0hg6acahj44xi485fsnrz1"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f                  ;No test suite.
           #:imported-modules
           (append %pyproject-build-system-modules
                   %cargo-build-system-modules)
           #:modules
           '(((guix build cargo-build-system) #:prefix cargo:)
             (guix build pyproject-build-system)
             (guix build utils))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'prepare-cargo-build-system
                 (lambda args
                   (for-each
                    (lambda (phase)
                      (format #t "Running cargo phase: ~a~%" phase)
                      (apply (assoc-ref cargo:%standard-phases phase)
                             #:cargo-target #$(cargo-triplet)
                             args))
                    '(unpack-rust-crates
                      configure
                      check-for-pregenerated-files
                      patch-cargo-checksums)))))))
    (native-inputs
     (append
      (list python-setuptools
            python-setuptools-rust
            python-wheel
            rust
            `(,rust "cargo") )
      (or (and=> (%current-target-system)
                 (compose list make-rust-sysroot))
          '())))
    (inputs (rosenthal-cargo-inputs 'python-cryptg))
    (home-page "https://github.com/cher-nov/cryptg")
    (synopsis "Cryptographic utilities for Telegram")
    (description
     "This is a small native extension for Python 3 to help libraries that want
to work with the Telegram API, which uses the uncommon AES-IGE mode for it.")
    (license license:cc0)))

(define-public python-mautrix
  (package
    (name "python-mautrix")
    (version "0.20.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/mautrix/python")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1axwcdrs3zws5jgxxxpf7309p6bc8hrwr0jy94yc1km7kqjvq8l2"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-aiosqlite
           python-asyncpg
           python-olm
           python-pycryptodome
           python-pytest
           python-pytest-asyncio
           python-ruamel.yaml
           python-setuptools
           python-unpaddedbase64))
    (propagated-inputs
     (list python-aiohttp python-attrs python-yarl))
    (home-page "https://github.com/mautrix/python")
    (synopsis "Python 3 asyncio Matrix framework")
    (description
     "This package provides a Python 3.10+ asyncio Matrix framework.")
    (license license:mpl2.0)))
