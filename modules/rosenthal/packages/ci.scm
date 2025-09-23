;;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages ci)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (gnu packages ci))

(define-public cuirass/dolly
  (let ((commit "6ea7d8d94010a00548968fcc3dd493af383c4000")
        (revision "1"))
    (package
      (inherit cuirass)
      (name "cuirass-dolly")
      (version (git-version "1.3.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://git.guix.gnu.org/cuirass.git")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1clzkzclzqfivv7cpwh8r20j8j5nzfnzbf1rrnhcyc1gvlds7blb")))))))
