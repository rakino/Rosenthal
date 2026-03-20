;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2022, 2025 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal packages package-management)
  ;; Guix utilities
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (rosenthal utils packages)
  ;; Guix packages
  #:use-module (gnu packages package-management))

(define-public guix/dolly
  (package
    (inherit
     (package-with-extra-patches guix
       (rosenthal-patches "guix-change-publish-cache-storage.patch"
                          "guix-allow-out-of-tree-modules-in-initrd.patch"
                          "guix-wip-zfs-boot-support.patch")))
    (name "guix-dolly")
    (arguments
     (substitute-keyword-arguments arguments
       ((#:parallel-build? _ #f) #t)))
    (properties '((disable-updater? . #t)))))
