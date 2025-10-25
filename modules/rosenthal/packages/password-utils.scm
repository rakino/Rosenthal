;;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages password-utils)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (rosenthal utils download)
  #:use-module (guix build-system go)
  #:use-module (gnu packages golang))

(define-public sops
  (package
    (name "sops")
    (version "3.11.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/getsops/sops")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1md6mk1cklcaa90mfy5dsm44i5xc0l598dfr3mi0j1k4ydkfn280"))))
    (build-system go-build-system)
    (arguments
     (list #:go go-1.24
           #:install-source? #f
           #:import-path "./cmd/sops"
           #:build-flags
           #~(list (string-append
                    "-ldflags="
                    "-X github.com/getsops/sops/v3/version.Version="
                    #$(package-version this-package)))
           #:modules
           '(((guix build gnu-build-system) #:prefix gnu:)
             (guix build go-build-system)
             (guix build utils))
           #:phases
           #~(modify-phases %standard-phases
               (replace 'unpack
                 (lambda args
                   (unsetenv "GO111MODULE")
                   (apply (assoc-ref gnu:%standard-phases 'unpack) args)
                   (copy-recursively
                    #+(this-package-native-input "vendored-go-dependencies")
                    "vendor")))
               (replace 'install-license-files
                 (assoc-ref gnu:%standard-phases 'install-license-files)))))
    (native-inputs
     (list (origin
             (method (go-mod-vendor #:go go-1.24))
             (uri (package-source this-package))
             (file-name "vendored-go-dependencies")
             (sha256
              (base32
               "05pd6d80x70xyak8clnjiiir8limlzq5cx2aqlda3p1ln4qpgc7m")))))
    (home-page "https://getsops.io/")
    (synopsis "Simple and flexible tool for managing secrets")
    (description
     "@acronym{SOPS, Secrets OPerationS} is an editor of encrypted files that
supports YAML, JSON, ENV, INI and binary formats and encrypts with @acronym{AWS
KMS, Amazon Web Services Key Management Service}, @acronym{GCP KMS, Google Cloud
Platform Key Management Service}, Azure Key Vault, @code{age}, and OpenPGP.")
    (license license:mpl2.0)))
