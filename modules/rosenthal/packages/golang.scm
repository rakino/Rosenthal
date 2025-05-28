;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages golang)
  #:use-module (guix packages)
  #:use-module ((guix build utils) #:select (alist-replace))
  #:use-module (guix git-download)
  #:use-module (gnu packages golang))

(define-public go-cloudflare
  (let ((commit "37bc41c6ff79507200a315b72834fce6ca427a7e")
        (revision "0"))
    (package
      (inherit go-1.22)
      (name "go-cloudflare")
      (version (git-version "1.22.12" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/cloudflare/go")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1zg6jqwhj42gaapk1fzqc4i7a6shdbfbpqgqhjyry55r4i0nqvxy"))))
      (home-page "https://github.com/cloudflare/go")
      (synopsis "Go with Cloudflare experimental patches")
      (properties
       `(,@(package-properties go-1.22)
         (disable-updater? . #t))))))
