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
                ,(if (content-hash? vendor-hash)
                     (origin
                       (method (go-mod-vendor #:go go))
                       (uri source)
                       (hash vendor-hash))
                     (origin
                       (method (go-mod-vendor #:go go))
                       (uri source)
                       (sha256 vendor-hash)))))
             '())
       ,@(bag-build-inputs go-bag)))
    (arguments
     (strip-keyword-arguments private-keywords (bag-arguments go-bag)))
    (build
     (lambda args
       (apply (bag-build go-bag)
              (let loop ((positional '())
                         (keyword '())
                         (args args))
                (match args
                  (((? keyword? key) (? (negate keyword?) val) rest ...)
                   (loop positional
                         (cons* key val keyword)
                         rest))
                  ((arg rest ...)
                   (loop (cons arg positional)
                         keyword
                         rest))
                  (()
                   (append (reverse positional)
                           (substitute-keyword-arguments keyword
                             ((#:imported-modules imported-modules '())
                              (if (null? imported-modules)
                                  %default-go-vendored-imported-modules
                                  imported-modules))
                             ((#:modules modules '())
                              (if (null? modules)
                                  %default-go-vendored-modules
                                  modules))))))))))))

(define go-vendored-build-system
  (build-system
    (name 'go-vendored)
    (description
     "Build system for Go programs, with vendored dependencies")
    (lower lower)))
