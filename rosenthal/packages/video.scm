;; SPDX-FileCopyrightText: 2022 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages video)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
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
         (delete "libx11" "libxext" "libxfixes"))))))
