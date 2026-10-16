;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal build-system go-vendored)
  #:use-module (ice-9 match)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (rosenthal utils download)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:export (%default-go-vendored-imported-modules
            %default-go-vendored-modules
            go-vendored-build-system))

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

(define* (lower name #:key source go vendor-hash #:allow-other-keys #:rest rest)
  (define private-keywords
    '(#:vendor-hash))

  (define go-lower
    (build-system-lower go-build-system))

  (define go-bag
    (apply go-lower name
           #:source source
           #:go go
           rest))

  (bag
    (inherit go-bag)
    (build-inputs
     `(,@(if (and source vendor-hash)
             `(("vendored-go-dependencies"
                ,(origin
                   (method (go-mod-vendor #:go go))
                   (uri source)
                   (file-name (string-append name "-vendored-go-dependencies"))
                   (hash
                    (if (content-hash? vendor-hash)
                        vendor-hash
                        (content-hash vendor-hash))))))
             '())
       ,@(bag-build-inputs go-bag)))
    (build (bag-build go-bag))
    (arguments
     (substitute-keyword-arguments
         (strip-keyword-arguments private-keywords (bag-arguments go-bag))
       ((#:imported-modules imported-modules '())
        (if (null? imported-modules)
            %default-go-vendored-imported-modules
            imported-modules))
       ((#:modules modules '())
        (if (null? modules)
            %default-go-vendored-modules
            modules))))))

(define go-vendored-build-system
  (build-system
    (name 'go-vendored)
    (description
     "Build system for Go programs, with vendored dependencies")
    (lower lower)))
