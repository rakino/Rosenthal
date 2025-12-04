;;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages animation)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages animation)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages image)
  #:use-module (guix build-system cmake)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))


(define-public lottie-converter
  (let ((commit "3ad34af7bbcf9197fda782a5832b18b72a3bb939")
        (revision "0"))
    (package
      (name "lottie-converter")
      (version (git-version "0.2" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/sot-tech/LottieConverter")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "12py6y05cv38cri0q813m7nsaizhqjnqnq0jp6bfviam5cq240p8"))))
      (build-system cmake-build-system)
      (arguments
       (list #:configure-flags
             #~(list "-DSYSTEM_PNG=1"
                     "-DSYSTEM_RL=1"
                     "-DSYSTEM_GL=1")
             #:phases
             #~(modify-phases %standard-phases
                 (replace 'install
                   (lambda _
                     (let ((bindir (in-vicinity #$output "bin")))
                       (install-file "lottieconverter" bindir)))))))
      (inputs (list giflib libpng rlottie zlib))
      (synopsis "Lottie animation converter")
      (description "This package provides a simple lottie animation converter.
Animations can be converted to PNG (with transparency) or GIF.")
      (home-page "https://github.com/sot-tech/LottieConverter")
      (license license:bsd-3)
      (properties '((disable-updater? . #t))))))
