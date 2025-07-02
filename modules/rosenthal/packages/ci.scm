;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages ci)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (gnu packages ci))

(define-public cuirass/hako
  (let ((commit "37f6066f5385d11a05c1253757570a1ce4ade7bc")
        (revision "1"))
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
           "01k2xpj1xhwbbg7ppglkb29x2wlp0i01y1r8bkql4763gah07wc1"))))
      (properties '((disable-updater? . #t))))))
