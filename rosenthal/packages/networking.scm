;; SPDX-FileCopyrightText: 2022 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages networking)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system go)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages golang)
  #:use-module (rosenthal packages golang))

(define-public cloudflared
  (package
    (name "cloudflared")
    (version "2023.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/cloudflare/cloudflared")
                    (commit version)))
              (file-name (git-file-name name version))
              ;; TODO: Unbundle vendored dependencies.
              ;; (modules '((guix build utils)))
              ;; (snippet '(delete-file-recursively "vendor"))
              (sha256
               (base32
                "1gnq7ys4hbi1x3an0ch4r7ynrk20lnb9jv01cim94v57s7dw2xvs"))))
    (build-system go-build-system)
    (arguments
     (list #:go go-1.19
           #:install-source? #f
           #:import-path "github.com/cloudflare/cloudflared/cmd/cloudflared"
           #:unpack-path "github.com/cloudflare/cloudflared"))
    (home-page "https://developers.cloudflare.com/cloudflare-one/connections/connect-apps/")
    (synopsis "Cloudflare Tunnel client")
    (description
     "This package provides the command-line client for Cloudflare Tunnel, a
tunneling daemon that proxies traffic from the Cloudflare network to your
origins.  This daemon sits between Cloudflare network and your origin (e.g. a
webserver).  Cloudflare attracts client requests and sends them to you via
this daemon, without requiring you to poke holes on your firewall --- your
origin can remain as closed as possible.")
    (license license:asl2.0)))

;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=60509
(define-public phantomsocks
  (package
    (name "phantomsocks")
    (version "0.0.0-20221222155609-14291e2c889e")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/macronut/phantomsocks")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "13lllmh46xwns5ksqqdkl2p7vvnbzkzb6va005nk37bx6c4x2ixp"))))
    (build-system go-build-system)
    (arguments
     (list #:install-source? #f
           #:import-path "github.com/macronut/phantomsocks"
           #:build-flags #~'("-tags" #$(if (target-linux?)
                                           "rawsocket"
                                           "pcap"))))
    (propagated-inputs
     (list go-github-com-google-gopacket
           go-github-com-macronut-go-tproxy))
    (inputs
     (if (target-linux?)
         '()
         (list libpcap)))
    (home-page "https://github.com/macronut/phantomsocks")
    (synopsis "Internet censorship circumvention tool")
    (description
     "Phantomsocks is an Internet censorship circumvention tool based on the
desync technique, which was introduced in the 2017 paper
@url{https://doi.org/10.1145/3131365.3131374, @cite{Your State is Not Mine: A
Closer Look at Evading Stateful Internet Censorship}}.

Further information on the usage could be found on the Wikibooks page
@url{https://zh.wikibooks.org/wiki/Phantomsocks, @cite{Phantomsocks}}.")
    (license license:lgpl3)))
