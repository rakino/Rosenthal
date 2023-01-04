;; SPDX-FileCopyrightText: 2022 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages golang)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system go)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public go-github-com-google-gopacket
  (package
    (name "go-github-com-google-gopacket")
    (version "1.1.19")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/google/gopacket")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "048qwm2n0wrpql4qqgd7jyynn3gk069yvqbxnshlayzmbhf87ls4"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/google/gopacket"))
    (home-page "https://github.com/google/gopacket")
    (synopsis "Packet processing capabilities library")
    (description
     "This package provides packet processing capabilities for Go.")
    (license license:bsd-3)))

(define-public go-github-com-macronut-go-tproxy
  (package
    (name "go-github-com-macronut-go-tproxy")
    (version "0.0.0-20190726054950-ef7efd7f24ed")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/FutureProtocolLab/go-tproxy")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0jibsg0xhsn0h1jq4g9qd4nr58w43y8majlwfri9ffk2cbfrwqdr"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/macronut/go-tproxy"))
    (home-page "https://github.com/FutureProtocolLab/go-tproxy")
    (synopsis "Linux Transparent Proxy library")
    (description
     "Golang TProxy provides an easy to use wrapper for the Linux Transparent
Proxy functionality.")
    (license license:expat)))
