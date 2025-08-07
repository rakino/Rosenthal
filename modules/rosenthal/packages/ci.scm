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
  (let ((commit "ccc11de138b5c15990551ad6cc883aeb15a8f80c")
        (revision "2"))
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
           "1yxfss23pkr39ymrcw3injqm05aqczhkyjrn79qkfakwi2bqismm"))))
      (properties '((disable-updater? . #t))))))
