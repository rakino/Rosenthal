;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages rust-apps)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (gnu packages crates-crypto)
  #:use-module (rosenthal packages rust-crates))

(define-public atuin
  (package
    (name "atuin")
    (version "18.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/atuinsh/atuin")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1zi7ar999ycvig9c9crylab540xdgr0h6v99q9j8ypk9i1fviyiz"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:features
      ''("client" "sync" "server" "clipboard" "daemon")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-failing-tests
            (lambda _
              (substitute* '("crates/atuin/tests/sync.rs"
                             "crates/atuin/tests/users.rs"
                             "crates/atuin-dotfiles/src/store.rs"
                             "crates/atuin-dotfiles/src/store/var.rs")
                (((string-append
                   ".*async fn (" (string-join
                                   '(;; Require running database.
                                     "build_aliases"
                                     "build_vars"
                                     "sync"
                                     "registration"
                                     "change_password"
                                     "multi_user_test")
                                   "|") ")")
                  all)
                 (string-append "#[ignore]\n" all)))))
          (add-after 'unpack 'patch-references
            (lambda _
              (substitute* (find-files "crates/atuin/src/shell")
                (("atuin (uuid|history|search)" all)
                 (string-append #$output "/bin/" all)))))
          (replace 'install
            (lambda* (#:key outputs features #:allow-other-keys)
              (let* ((out      (assoc-ref outputs "out"))
                     (registry (string-append out "/share/cargo/registry"))
                     (sources  (string-append out "/share/cargo/src")))
                (mkdir-p out)
                ;; Make cargo reuse all the artifacts we just built instead
                ;; of defaulting to making a new temp directory
                (setenv "CARGO_TARGET_DIR" "./target")
                ;; Only install crates which include binary targets,
                ;; otherwise cargo will raise an error.
                (invoke "cargo" "install" "--no-track" "--path" "crates/atuin"
                        "--root" out "--features" (string-join features))))))))
    (inputs (cons* rust-ring-0.17 atuin-cargo-inputs))
    (home-page "https://atuin.sh/")
    (synopsis "Sync, search and backup shell history")
    (description
     "Atuin replaces existing shell history with a SQLite database, and records
additional context for commands.  Additionally, it provides optional and fully
encrypted synchronisation of history between machines, via an Atuin server.")
    (license license:expat)))
