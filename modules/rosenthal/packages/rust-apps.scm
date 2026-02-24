;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2025 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal packages rust-apps)
  ;; Utilities
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (rosenthal utils cargo)
  ;; Guix origin methods
  #:use-module (guix git-download)
  ;; Guix build systems
  #:use-module (guix build-system cargo)
  ;; Guix packages
  #:use-module (gnu packages sqlite))

(define-public atuin
  (package
    (name "atuin")
    (version "18.12.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/atuinsh/atuin")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0gm7b4xzvks3fs0i6ijaajq47djr0ydfyac32c2liqdvikq0ncm2"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:tests? #f                       ;TODO.
      #:install-source? #f
      #:features
      ''("client" "sync" "clipboard" "daemon")
      #:cargo-install-paths
      ''("crates/atuin")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-references
            (lambda _
              (substitute* (find-files "crates/atuin/src/shell")
                (("atuin (uuid|history|search)" all)
                 (string-append #$output "/bin/" all))))))))
    (inputs (cons sqlite (rosenthal-cargo-inputs 'atuin)))
    (home-page "https://atuin.sh/")
    (synopsis "Sync, search and backup shell history")
    (description
     "Atuin replaces existing shell history with a SQLite database, and records
additional context for commands.  Additionally, it provides optional and fully
encrypted synchronisation of history between machines, via an Atuin server.")
    (license license:expat)))
