;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal packages version-control)
  ;; Utilities
  #:use-module (guix packages)
  #:use-module (rosenthal utils packages)
  ;; Guix packages
  #:use-module (gnu packages rust-apps))

(define-public jujutsu/dolly
  (package
    (inherit
     (package-with-extra-patches jujutsu
       (rosenthal-patches
        "jujutsu-save-jjdescription-file-inside-repository.patch")))
    (name "jujutsu-dolly")))
