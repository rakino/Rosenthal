;;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages ci)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (rosenthal utils packages)
  #:use-module (gnu packages ci))

(define-public cuirass/dolly
  (package
    (inherit
     (package-with-extra-patches cuirass
       (rosenthal-patches "cuirass-templates-Add-more-forges.patch")))
    (name "cuirass-dolly")
    (properties '((disable-updater? . #t)))))
