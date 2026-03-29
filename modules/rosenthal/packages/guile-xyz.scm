;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal packages guile-xyz)
  ;; Utilities
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (rosenthal utils packages)
  ;; Guix origin methods
  #:use-module (guix git-download)
  ;; Guix build systems
  #:use-module (guix build-system guile)
  ;; Guix packages
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz))

(define-public guile-toml
  (let ((commit "ecb24deb407ef76ef7cf7e9f0115060c98366a6b")
        (revision "0"))
    (package
      (name "guile-toml")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/hylophile/guile-toml")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1iqxivxcb0dcjbd5ba3c0g3mj71b32qjxfdmljyv9r297yykfd02"))
                (patches
                 (rosenthal-patches "guile-toml-support-boolean.patch"))))
      (build-system guile-build-system)
      (arguments
       (list #:phases
             #~(modify-phases %standard-phases
                 (add-after 'unpack 'avoid-test-installation
                   (lambda _
                     (delete-file-recursively "test"))))))
      (native-inputs (list guile-3.0))
      (propagated-inputs (list guile-json-4))
      (home-page "https://github.com/hylophile/guile-toml")
      (synopsis "TOML module for GNU Guile")
      (description "")
      (license license:gpl3+)
      (properties '((disable-updater? . #t))))))
