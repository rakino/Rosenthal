;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2016 Petter <petter@mykolab.ch>
;;; Copyright © 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2021-2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2021, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2024 Christina O'Donnell <cdo@mutix.org>
;;; Copyright © 2024 Troy Figiel <troy@troyfigiel.com>
;;; Copyright © 2024 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2026 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal build-system go-vendored)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix packages)
  #:use-module (rosenthal utils download)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:export (%default-go-vendored-imported-modules
            %default-go-vendored-modules
            go-vendored-build-system))

;; Commentary:
;;
;; Standard build procedure for packages using the Go build system.  It is
;; implemented as an extension of 'gnu-build-system'.
;;
;; Code:

(define (go-target target)
  ;; Parse the nix-system equivalent of the target and set the
  ;; target for compilation accordingly.
  (match (string-split (gnu-triplet->nix-system target) #\-)
    ((arch os)
     (list (match arch
             ("aarch64" "arm64")
             ("armhf" "arm")
             ("powerpc64le" "ppc64le")
             ("powerpc64" "ppc64")
             ("i686" "386")
             ("x86_64" "amd64")
             ("mips64el" "mips64le")
             ("loongarch64" "loong64")
             (_ arch))
           (match os
             ((or "mingw32" "cygwin") "windows")
             ("gnu" "hurd")
             (_ os))))
    (_
     (raise
      (condition
       (&unsupported-cross-compilation-target-error
        (build-system go-vendored-build-system)
        (target target)))))))

(define %default-go-vendored-imported-modules
  ;; Build-side modules imported and used by default.
  `(,@%default-gnu-imported-modules
    (guix build union)
    (guix build go-build-system)
    (rosenthal build go-vendored-build-system)))

(define %default-go-vendored-modules
  '((guix build union)
    (guix build utils)
    (rosenthal build go-vendored-build-system)))

(define (default-go)
  (@* (gnu packages golang) go))

(define (default-gccgo)
  (@* (gnu packages gcc) gccgo-15))

(define (make-go-std)
  (@* (gnu packages golang) make-go-std))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (go (if (supported-package? (default-go))
                        (default-go)
                        (default-gccgo)))
                vendor-hash
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    '(#:target #:go #:vendor-hash #:inputs #:native-inputs))

  (define inputs-with-cache
    ;; XXX: Avoid a circular dependency.  This should be rewritten with
    ;; 'package-mapping' or similar.
    (let ((go-std-name (string-append (package-name go) "-std")))
      (if (string-prefix? go-std-name name)
          inputs
          (cons `(,go-std-name ,((make-go-std) go)) inputs))))

  (bag
    (name name)
    (system system)
    (target target)
    (build-inputs `(,@(if source
                          `(("source" ,source))
                          '())
                    ,@(if (and source vendor-hash)
                          `(("vendored-go-dependencies"
                             ,(origin
                                (method (go-mod-vendor #:go go))
                                (uri source)
                                (sha256 vendor-hash))))
                          '())
                    ,@native-inputs
                    ,@`(("go" ,go))
                    ,@(if target '() inputs-with-cache)
                    ,@(if target
                          ;; Use the standard cross inputs of
                          ;; 'gnu-build-system'.
                          (standard-cross-packages target 'host)
                          '())
                    ;; Keep the standard inputs of 'gnu-build-system'.
                    ,@(standard-packages)))
    (host-inputs (if target inputs-with-cache '()))

    ;; The cross-libc is really a target package, but for bootstrapping
    ;; reasons, we can't put it in 'host-inputs'.  Namely, 'cross-gcc' is a
    ;; native package, so it would end up using a "native" variant of
    ;; 'cross-libc' (built with 'gnu-build'), whereas all the other packages
    ;; would use a target variant (built with 'gnu-cross-build'.)
    (target-inputs (if target
                       (standard-cross-packages target 'target)
                       '()))

    (outputs outputs)
    (build (if target go-vendored-cross-build go-vendored-build))
    (arguments (strip-keyword-arguments private-keywords arguments))))

(define* (go-vendored-build name inputs
                            #:key
                            source
                            (phases '%standard-phases)
                            (outputs '("out"))
                            (search-paths '())
                            (install-source? #t)
                            (embed-files ''())
                            (import-path ".")
                            (unpack-path "")
                            (build-flags ''())
                            (skip-build? #f)
                            (tests? #t)
                            (test-flags ''())
                            (test-subdirs ''("..."))
                            (parallel-build? #t)
                            (parallel-tests? #t)
                            (allow-go-reference? #f)
                            (system (%current-system))
                            (goarch #f)
                            (goos #f)
                            (guile #f)
                            (imported-modules %default-go-vendored-imported-modules)
                            (modules %default-go-vendored-modules)
                            (substitutable? #t))
  (define builder
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@modules)
          (go-vendored-build #:name #$name
                             #:source #+source
                             #:system #$system
                             #:phases #$phases
                             #:outputs #$(outputs->gexp outputs)
                             #:goarch #$goarch
                             #:goos #$goos
                             #:embed-files #$embed-files
                             #:search-paths '#$(sexp->gexp
                                                (map search-path-specification->sexp
                                                     search-paths))
                             #:install-source? #$install-source?
                             #:import-path #$import-path
                             #:unpack-path #$unpack-path
                             #:build-flags #$build-flags
                             #:skip-build? #$skip-build?
                             #:tests? #$tests?
                             #:test-flags #$test-flags
                             #:test-subdirs #$test-subdirs
                             #:parallel-build? #$parallel-build?
                             #:parallel-tests? #$parallel-tests?
                             #:allow-go-reference? #$allow-go-reference?
                             #:inputs #$(input-tuples->gexp inputs)))))

  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system #:graft? #f)))
    (gexp->derivation name builder
                      #:system system
                      #:graft? #f
                      #:substitutable? substitutable?
                      #:guile-for-build guile)))

(define* (go-vendored-cross-build name
                                  #:key
                                  source target
                                  build-inputs target-inputs host-inputs
                                  (phases '%standard-phases)
                                  (outputs '("out"))
                                  (search-paths '())
                                  (native-search-paths '())
                                  (install-source? #t)
                                  (import-path ".")
                                  (unpack-path "")
                                  (build-flags ''())
                                  (skip-build? #f)
                                  (tests? #f)              ; nothing can be done
                                  (test-flags ''())
                                  (test-subdirs ''("..."))
                                  (parallel-build? #t)
                                  (parallel-tests? #t)
                                  (allow-go-reference? #f)
                                  (system (%current-system))
                                  (goarch (first (go-target target)))
                                  (goos (last (go-target target)))
                                  (embed-files ''())
                                  (guile #f)
                                  (imported-modules %default-go-vendored-imported-modules)
                                  (modules %default-go-vendored-modules)
                                  (substitutable? #t))
  "Cross-build NAME using GO, where TARGET is a GNU triplet and with INPUTS."
  (define builder
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@(sexp->gexp modules))

          (define %build-host-inputs
            #+(input-tuples->gexp build-inputs))

          (define %build-target-inputs
            (append #$(input-tuples->gexp host-inputs)
              #+(input-tuples->gexp target-inputs)))

          (define %build-inputs
            (append %build-host-inputs %build-target-inputs))

          (define %outputs
            #$(outputs->gexp outputs))

          (go-vendored-build #:name #$name
                             #:source #+source
                             #:system #$system
                             #:phases #$phases
                             #:outputs %outputs
                             #:target #$target
                             #:goarch #$goarch
                             #:goos #$goos
                             #:embed-files #$embed-files
                             #:inputs %build-target-inputs
                             #:native-inputs %build-host-inputs
                             #:search-paths '#$(map search-path-specification->sexp
                                                    search-paths)
                             #:native-search-paths '#$(map
                                                       search-path-specification->sexp
                                                       native-search-paths)
                             #:install-source? #$install-source?
                             #:import-path #$import-path
                             #:unpack-path #$unpack-path
                             #:build-flags #$build-flags
                             #:skip-build? #$skip-build?
                             #:tests? #$tests?
                             #:test-flags #$test-flags
                             #:test-subdirs #$test-subdirs
                             #:parallel-build? #$parallel-build?
                             #:parallel-tests? #$parallel-tests?
                             #:make-dynamic-linker-cache? #f ;cross-compiling
                             #:allow-go-reference? #$allow-go-reference?
                             #:inputs %build-inputs))))

  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system #:graft? #f)))
    (gexp->derivation name builder
                      #:system system
                      #:target target
                      #:graft? #f
                      #:substitutable? substitutable?
                      #:guile-for-build guile)))

(define go-vendored-build-system
  (build-system
    (name 'go)
    (description
     "Build system for Go programs")
    (lower lower)))
