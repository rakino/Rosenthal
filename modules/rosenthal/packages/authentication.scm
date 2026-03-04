;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal packages authentication)
  ;; Utilities
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (rosenthal utils packages)
  ;; Guix origin methods
  #:use-module (guix git-download)
  ;; Guix build systems
  #:use-module (guix build-system qt)
  ;; Guix packages
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages qt))

(define-public bb-auth
  (package
    (name "bb-auth")
    (version "0.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/anthonyhab/bb-auth")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1jx526h7qw7mw4cp2c8ys4ck1qlc5v9qf51zvj951l4kqbrs0w44"))
              (patches (rosenthal-patches "bb-auth.patch"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-test-references
                 (lambda _
                   (substitute* (find-files "tests")
                     (("/bin/(true|false)" _ cmd)
                      (which cmd)))))
               (add-after 'qt-wrap 'patch-script
                 (lambda _
                   (with-directory-excursion (in-vicinity #$output "libexec")
                     (substitute* "pinentry-bb"
                       (("libexec/bb-auth\"" all)
                        (string-append all " \"--pinentry\"")))
                     (substitute* "bb-keyring-prompter"
                       (("libexec/bb-auth\"" all)
                        (string-append all " \"--keyring\"")))))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list gcr
           json-glib-minimal
           polkit
           polkit-qt6))
    (home-page "https://github.com/anthonyhab/bb-auth")
    (synopsis "Unified polkit, keyring, and pinentry authentication daemon")
    (description "")
    (license license:bsd-3)))
