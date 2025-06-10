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
  (let ((commit "dfbc024c7d4865c1d7f91017bcdb6c1b3e8dffd9")
        (revision "0"))
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
           "0n713faaz8qbi4c3k7y6iz494jya8pbjysqgxiww4wr3y7vhhx9n"))))
      (properties '((disable-updater? . #t))))))
