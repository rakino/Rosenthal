;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2025 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal packages ci)
  ;; Utilities
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (rosenthal utils packages)
  ;; Guix origin methods
  #:use-module (guix git-download)
  ;; Guix packages
  #:use-module (gnu packages ci))

(define-public cuirass/dolly
  (package
    (inherit
     (package-with-extra-patches cuirass
       (rosenthal-patches "cuirass-templates-Add-more-forges.patch")))
    (name "cuirass-dolly")
    (properties '((disable-updater? . #t)))))
