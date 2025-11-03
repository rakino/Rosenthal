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
  #:use-module (gnu packages time)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (rosenthal utils cargo))

(define-public python-coherent-licensed
  (package
    (name "python-coherent-licensed")
    (version "0.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "coherent_licensed" version))
       (sha256
        (base32 "0va113rfa0i8sns2if8mnkzwliks0nvlzp1db71klbblrq1i81yq"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-flit-core))
    (home-page "https://pypi.org/project/coherent.licensed/")
    (synopsis "License management tooling")
    (description
     "This library was built for
@url{https://github.com/coherent-oss/coherent.build, coherent.build} and
@url{https://blog.jaraco.com/skeleton, skeleton} projects to inject a license
file at build time to reflect the license declared in the license expression.")
    (license license:expat)))

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

(define-public python-irc
  (package
    (name "python-irc")
    (version "20.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "irc" version))
       (sha256
        (base32 "02r8fb15wb6b9rf4kh3cvs48a4j00slncj71s4g152sdi6pq6sw0"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-setuptools
           python-setuptools-scm
           python-pytest))
    (propagated-inputs
     (list python-jaraco-collections
           python-jaraco-functools
           python-jaraco-logging
           python-jaraco-stream
           python-jaraco-text
           python-more-itertools
           python-pytz
           python-tempora))
    (home-page "https://github.com/jaraco/irc")
    (synopsis "Python Internet Relay Chat protocol library")
    (description
     "This library provides a low-level implementation of the @acronym{IRC,
Internet Relay Chat} protocol for Python.  It provides an event-driven IRC
client framework.  It has a fairly thorough support for the basic IRC protocol,
@acronym{CTCP, Client-to-Client Protocol}, and @acronym{DCC, Direct
Client-to-Client} connections.")
    (license license:expat)))

(define-public python-jaraco-logging
  (package
    (name "python-jaraco-logging")
    (version "3.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jaraco_logging" version))
       (sha256
        (base32 "1yvmnyq120sqfcbzvqrcjk5a35jqh0fqbvfnn6gfcz27d21xrmp7"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-coherent-licensed
           python-pytest
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-tempora))
    (home-page "https://github.com/jaraco/jaraco.logging")
    (synopsis "Python logging facility")
    (description
     "This package provides additional facilities to supplement Python's
standard logging module.")
    (license license:expat)))

(define-public python-jaraco-stream
  (package
    (name "python-jaraco-stream")
    (version "3.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jaraco_stream" version))
       (sha256
        (base32 "0jwk85k14zb1pvppxy6vnbln8xib7yiwjvmrab42rv91wwl51g72"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-more-itertools))
    (home-page "https://github.com/jaraco/jaraco.stream")
    (synopsis "Routines for dealing with data streams")
    (description
     "This package provides routines for handling streaming data, including a
set of generators for loading gzip data on the fly.")
    (license license:expat)))

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

(define-public python-tulir-telethon
  (package
    (name "python-tulir-telethon")
    (version "1.99.0a6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/tulir/Telethon")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1z53na08wqhx5fk5z22j1j34vl2q07d1f9knrn9k6r4v2bxw0nds"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f))      ;TODO
    (native-inputs (list python-setuptools python-wheel))
    (propagated-inputs (list python-cryptg python-pyaes python-rsa))
    (home-page "https://github.com/tulir/Telethon")
    (synopsis "Python 3 MTProto API Telegram client library")
    (description
     "Telethon is an @code{asyncio} Python 3 MTProto library to interact with
Telegram's API as a user or through a bot account (bot API alternative).")
    (license license:expat)))
