;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Hilton Chain <hako@ultrarare.space>

(define-module (rosenthal packages file-systems)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (gnu packages file-systems))

(define-public zfs-cachyos
  (let ((commit "fb48c76e17d3e0092fbba430a9eec1960e1b9961")
        (revision "0"))
    (package/inherit zfs
      (name "zfs-cachyos")
      (version (git-version "2.4.5" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://github.com/CachyOS/zfs")
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1zb3rs40fhkhn2fd3g5aspm50cy4xvg84ah84h5brz6abvfr1rg4")))))))
