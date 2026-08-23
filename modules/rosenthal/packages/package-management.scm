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

(define guix-for-mirror-substitutes
  (let ((commit "64d4de2a920445e5992f020e56490f5fcbdbba7c")
        (revision "6"))
    (package
      (inherit guix)
      (name "guix")
      (version (git-version "1.5.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://git.guix.gnu.org/guix.git")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1j3gz77rrlg62ffxqacg1r0rhibmhbhkh0ygh27ambj495i0mi0b"))))
      (arguments
       (substitute-keyword-arguments arguments
         ((#:parallel-build? _ #f) #t)
         ((#:tests? _ #t) #f))))))

(define-public mirror-substitutes
  (package
    (name "mirror-substitutes")
    (version "0.0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://codeberg.org/hako/mirror-substitutes.git")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1fbv24n12p83gp9l8nnjmymgqbwyvmy086fnfwxdg8c9c6rfbl4j"))))
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
           guix-for-mirror-substitutes))
    (inputs
     (list bash-minimal
           coreutils-minimal
           guix-for-mirror-substitutes))
    (home-page "https://codeberg.org/hako/mirror-substitutes")
    (synopsis "Mirror Guix substitutes")
    (description
     "This package provides a collection of commands to help mirror Guix / Nix
substitutes (binary cache) from any upstream server.")
    (license license:gpl3+)))
