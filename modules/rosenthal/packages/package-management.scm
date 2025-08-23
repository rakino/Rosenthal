;;; SPDX-FileCopyrightText: 2022, 2025 Hilton Chain <hako@ultrarare.space>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages package-management)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (rosenthal utils packages)
  #:use-module (gnu packages package-management))

(define-public guix/hako
  (package
    (inherit
     (package-with-extra-patches guix
       (rosenthal-patches "guix-change-publish-cache-storage.patch")))
    (name "guix-hako")
    (arguments
     (substitute-keyword-arguments (package-arguments guix)
       ((#:tests? _ #t) #f)
       ((#:parallel-build? _ #f) #t)))
    (properties '((disable-updater? . #t)))))
