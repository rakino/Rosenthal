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

(define-public guile-toml/dolly
  (package
    (inherit guile-toml)
    (name "guile-toml")
    (version "1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/hylophile/guile-toml")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nyxj9gz0v5n2whi5wikidz5d5a7hf181vwnlr0v1l67xvxhxv1d"))
       (patches
        (rosenthal-patches "guile-toml-support-boolean.patch"))))
    (properties '((disable-updater? . #t)))))
