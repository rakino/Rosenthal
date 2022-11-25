;; SPDX-FileCopyrightText: 2022 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages python-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system pyproject)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz))

;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=59270
(define-public python-pynixutil
  (package
    (name "python-pynixutil")
    (version "0.5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/nix-community/pynixutil")
                    (commit version)))
              (file-name (git-file-name name version))
              ;; Derivation test uses nix.
              (modules '((guix build utils)))
              (snippet '(delete-file "tests/test_drv.py"))
              (sha256
               (base32
                "1lnspcai7mqpv73bbd8kgyw63fxwgkwvfkl09b2bl5y2g2v7np6m"))))
    (build-system pyproject-build-system)
    (native-inputs (list poetry python-pytest))
    (home-page "https://github.com/nix-community/pynixutil")
    (synopsis "Utility functions for working with data from Nix in Python")
    (description
     "@code{pynixutil} provides functions for base32 encoding/decoding and
derivation parsing, namingly @code{b32decode()}, @code{b32encode()} and
@code{drvparse()}.")
    (license license:expat)))
