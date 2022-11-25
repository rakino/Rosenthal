;; SPDX-FileCopyrightText: 2022 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages video)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages video))

;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=59050
(define-public libva-2.16.0
  (package
    (inherit libva)
    (name "libva")
    (version "2.16.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/intel/libva"
                                  "/releases/download/" version
                                  "/libva-" version ".tar.bz2"))
              (sha256
               (base32
                "070aj9nw681a4m7f5xb662hhyib0w9q0i0s9v8vplh9cvfhaqpqi"))))))

(define-public libva-nox
  (let ((base libva-2.16.0))
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
