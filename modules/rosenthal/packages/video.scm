;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2022 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal packages video)
  ;; Utilities
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  ;; Guix packages
  #:use-module (gnu packages video))

(define-public libva-nox
  (let ((base libva))
    (package
      (inherit base)
      (name "libva-nox")
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:configure-flags configure-flags)
          #~(append #$configure-flags (list "--disable-glx")))
         ((#:phases _) #~%standard-phases)))
      (inputs
       (modify-inputs (package-inputs base)
         (delete "libx11" "libxext" "libxfixes")))
      (properties
       `(,@(package-properties base)
         (disable-updater? . #t))))))
