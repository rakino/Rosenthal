;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2022, 2025 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal packages package-management)
  ;; Guix utilities
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (rosenthal utils packages)
  ;; Guix origin methods
  #:use-module (guix git-download)
  ;; Guix build systems
  #:use-module (guix build-system guile)
  ;; Guix packages
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages package-management))

(define-public guix/dolly
  (package
    (inherit
     (package-with-extra-patches guix
       (rosenthal-patches "guix-change-publish-cache-storage.patch")))
    (name "guix-dolly")
    (arguments
     (substitute-keyword-arguments arguments
       ((#:parallel-build? _ #f) #t)
       ((#:tests? _ #t) #f)))
    (properties '((disable-updater? . #t)))))

(define-public mirror-substitutes
  (let ((commit "c05952a9e8e002383b9bc1a30970c2af8c847105")
        (revision "1"))
    (package
      (name "mirror-substitutes")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://codeberg.org/hako/mirror-substitutes.git")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1maj4a2wa4n8bikrj1d31mr28h3gy0iygbiy8livg0ykgvdxiai0"))))
      (build-system guile-build-system)
      (arguments
       (list
        #:source-directory "modules"
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'build 'install
              (lambda* (#:key inputs #:allow-other-keys)
                (define-values (module-dir object-dir)
                  (target-guile-scm+go #$output))
                (define bindir
                  (in-vicinity #$output "bin"))
                (with-directory-excursion "scripts"
                  (for-each
                   (lambda (script)
                     (substitute* script
                       (("/usr/(bin/env) -S guix" _ env)
                        (format #f "~a -S ~a"
                                (search-input-file inputs env)
                                (if (string-suffix? "query-substitutes" script)
                                    "guix"
                                    (search-input-file inputs "bin/guix")))))
                     (install-file script bindir)
                     (with-directory-excursion bindir
                       (wrap-program script
                         `("GUILE_LOAD_PATH" prefix
                           (,module-dir))
                         `("GUILE_LOAD_COMPILED_PATH" prefix
                           (,object-dir)))))
                   (find-files "."))))))))
      (native-inputs
       (list bash-minimal
             guile-3.0-latest
             guix))
      (inputs
       (list bash-minimal
             coreutils-minimal
             guix))
      (home-page "https://codeberg.org/hako/mirror-substitutes")
      (synopsis
       "Mirror Guix substitutes without relying on remote server implementation")
      (description
       "This package provides commands to mirror substitutes for Guix and Nix
without relying on the implementation of remote server.")
      (license license:gpl3+)
      (properties '((disable-updater? . #t))))))
