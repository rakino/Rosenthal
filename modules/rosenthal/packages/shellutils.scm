;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal packages shellutils)
  ;; Utilities
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  ;; Guix origin methods
  #:use-module (guix git-download)
  ;; Guix build systems
  #:use-module (rosenthal build-system go-vendored)
  ;; Guix packages
  #:use-module (gnu packages golang))

(define-public stinkpot
  (package
    (name "stinkpot")
    (version "0.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://tangled.org/oppi.li/stinkpot")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1mnxjg3krvdgxclh4bd757p8wgvx57s9i8zyrppgwhcps1r2ifw2"))))
    (build-system go-vendored-build-system)
    (arguments
     (list #:tests? #f                  ;no tests
           #:go go-1.27
           #:vendor-hash
           (base32 "1kqmw6c2h11wxx4a683k10rzrsydc1pbq5rp3d570nk8bl5c0lr1")
           #:install-source? #f
           #:import-path "."
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-references
                 (lambda _
                   (substitute* "main.go"
                     (("stinkpot (add|search)" all)
                      (string-append #$output "/bin/" all))))))))
    (home-page "https://tangled.org/oppi.li/stinkpot")
    (synopsis "Search shell history")
    (description
     "@command{stinkpot} is an sqlite backed shell-history searcher.")
    (license license:expat)))
