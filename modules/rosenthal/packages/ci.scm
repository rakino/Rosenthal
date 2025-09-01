;;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages ci)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (gnu packages ci))

(define-public cuirass/hako
  (let ((commit "ba77a7cc44894e959665941121c133b9b3304768")
        (revision "3"))
    (package
      (inherit cuirass)
      (name "cuirass-hako")
      (version (git-version "1.2.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://codeberg.org/guix/cuirass.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "17s1jah2k276klqxcc7yvcl46n4by2s1q4ipi9icssabfpv4s6yp"))))
      (properties '((disable-updater? . #t))))))
