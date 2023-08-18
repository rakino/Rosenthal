;; SPDX-FileCopyrightText: 2023 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages binaries)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages glib))

(define license
  (@@ (guix licenses) license))

(define-public clash-bin
  (package
    (name "clash-bin")
    (version "1.17.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/Dreamacro/clash/releases/download/v"
                    version "/clash-linux-amd64-v3-v" version ".gz"))
              (sha256
               (base32
                "0b3r6z46xky01aq6f9v7vj7n8bqai2rwwpk1mng6336s72fnz6ds"))))
    (build-system copy-build-system)
    (arguments
     (list #:install-plan
           #~'((#$(format #f "clash-linux-amd64-v3-v~a" version) "bin/clash"))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'fix-permission
                 (lambda _
                   (chmod (string-append #$output "/bin/clash") #o555))))))
    (supported-systems '("x86_64-linux"))
    (home-page "https://github.com/Dreamacro/clash")
    (synopsis "Rule-based tunnel in Go")
    (description
     "Clash is a cross-platform rule-based proxy utility that runs on the
network and application layer, supporting various proxy and anti-censorship
protocols out-of-the-box.")
    (license license:gpl3)))

(define-public cloudflare-warp-bin
  (package
    (name "cloudflare-warp-bin")
    (version "2023.7.40")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://pkg.cloudflareclient.com"
                                  "/pool/bookworm/main/c/cloudflare-warp/"
                                  "cloudflare-warp_" version "-1_amd64.deb"))
              (sha256
               (base32
                "00nb0ibf7cx4l7pdrpb458lnpp61blkvxbm1hndlypmldiajrrzl"))))
    (build-system copy-build-system)
    (arguments
     (list #:install-plan
           #~'(("bin" "bin" #:include ("warp-cli" "warp-svc")))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'unpack-deb
                 (lambda _
                   (let ((deb-pack
                          (format #f "cloudflare-warp_~a-1_amd64.deb" #$version)))
                     (invoke "ar" "-x" deb-pack)
                     (invoke "tar" "-xf" "data.tar.gz"))))
               (add-after 'install 'patch-elf
                 (lambda _
                   (let ((ld.so (string-append #$(this-package-input "glibc")
                                               #$(glibc-dynamic-linker)))
                         (rpath (string-join
                                 (list (string-append #$gcc:lib "/lib")
                                       (string-append
                                        #$(this-package-input "dbus") "/lib")
                                       (string-append
                                        #$(this-package-input "glibc") "/lib"))
                                 ":")))
                     (define (patch-elf file)
                       (format #t "Patching ~a ..." file)
                       (unless (string-contains file ".so")
                         (invoke "patchelf" "--set-interpreter" ld.so file))
                       (invoke "patchelf" "--set-rpath" rpath file)
                       (display " done\n"))
                     (for-each (lambda (file)
                                 (patch-elf file))
                               (find-files
                                (string-append #$output "/bin")))))))))
    (supported-systems '("x86_64-linux"))
    (native-inputs (list patchelf))
    (inputs (list dbus glibc))
    (home-page "https://1.1.1.1/")
    (synopsis "Cloudflare WARP client")
    (description
     "The Cloudflare WARP client allows individuals to have a faster, more
secure, and more private experience online.  The WARP client sits between your
device and the Internet, and has several connection modes to better suit
different needs.")
    (license
     (license "Nonfree"
              "https://www.cloudflare.com/application/terms/"
              "This is a nonfree license.  Check the URI for details."))))

(define-public hugo-bin
  (package
    (name "hugo-bin")
    (version "0.117.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/gohugoio/hugo" "/releases/download/v"
                    version "/hugo_extended_" version "_linux-amd64.tar.gz"))
              (sha256
               (base32
                "1a94yx1z7rgp2ifv5rwh5207mjg2z0r470pin7bcrqqcd0h684sx"))))
    (build-system copy-build-system)
    (arguments
     (list #:install-plan #~'(("hugo" "bin/"))
           #:phases
           #~(modify-phases %standard-phases
               (delete 'strip)
               (add-after 'install 'patch-elf
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let ((hugo (string-append #$output "/bin/hugo")))
                     (invoke "patchelf" "--set-interpreter"
                             (search-input-file inputs #$(glibc-dynamic-linker))
                             hugo)
                     (invoke "patchelf" "--set-rpath"
                             (dirname (search-input-file
                                       inputs "/lib/libgcc_s.so.1"))
                             hugo)))))))
    (supported-systems '("x86_64-linux"))
    (native-inputs (list patchelf))
    (inputs (list `(,gcc "lib") glibc))
    (home-page "https://gohugo.io/")
    (synopsis "Static site generator")
    (description
     "Hugo is a static site generator written in Go, optimized for speed and
designed for flexibility.")
    (license license:asl2.0)))

(define-public shadow-tls-bin
  (package
    (name "shadow-tls-bin")
    (version "0.2.23")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/ihciah/shadow-tls/releases/download/v"
                    version "/shadow-tls-x86_64-unknown-linux-musl"))
              (sha256
               (base32
                "0jdmnhhrn8mwh5jx2k8fvmigq7vjbzx6wxd5zhahlcfga1p2kixc"))))
    (build-system copy-build-system)
    (arguments
     (list #:install-plan
           #~'(("shadow-tls-x86_64-unknown-linux-musl" "bin/shadow-tls"))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'fix-permission
                 (lambda _
                   (chmod (string-append #$output "/bin/shadow-tls") #o555))))))
    (supported-systems '("x86_64-linux"))
    (home-page "https://www.ihcblog.com/a-better-tls-obfs-proxy/")
    (synopsis "Proxy to expose real tls handshake to the firewall")
    (description
     "Shadow TLS is a proxy to expose real tls handshake to the @acronym{MITM,
monster-in-the-middle}.")
    (license license:expat)))

(define-public sing-box-bin
  (package
    (name "sing-box-bin")
    (version "1.3.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/SagerNet/sing-box/releases/download/v"
                    version "/sing-box-" version "-linux-amd64v3.tar.gz"))
              (sha256
               (base32
                "07ql4im1g2bs6w4hsinjj76pj38cmr7ybyaab49g2jjr5vqw5w0h"))))
    (build-system copy-build-system)
    (arguments (list #:install-plan #~'(("sing-box" "bin/"))))
    (supported-systems '("x86_64-linux"))
    (home-page "https://sing-box.sagernet.org/")
    (synopsis "Universal proxy platform")
    (description
     "This package provides @code{sing-box}, a universal proxy platform.")
    (license license:gpl3+)))

(define-public tailscale-bin
  (package
    (name "tailscale-bin")
    (version "1.46.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://pkgs.tailscale.com"
                                  "/stable/tailscale_" version "_amd64.tgz"))
              (sha256
               (base32
                "0s8q0p9gvrd726vdv9jlf4w6caq687ssrzbj7vza389aix3q6caa"))))
    (build-system copy-build-system)
    (arguments
     (list #:install-plan
           #~'(("." "bin/" #:include ("tailscale" "tailscaled")))))
    (supported-systems '("x86_64-linux"))
    (home-page "https://tailscale.com/")
    (synopsis "Private WireGuard® networks made easy")
    (description
     "This package provides @{tailscale}, which brings an easy and secure way
to use WireGuard and 2FA.")
    (license license:bsd-3)))

(define-public wakapi-bin
  (package
    (name "wakapi-bin")
    (version "2.8.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/muety/wakapi/releases/download/"
                    version "/wakapi_linux_amd64.zip"))
              (sha256
               (base32
                "1qjmvihw9rnlvlmj5jv5nac3ki9y92z3hy7p39gp50x65ia1jwg4"))))
    (build-system copy-build-system)
    (arguments (list #:install-plan #~'(("wakapi" "bin/wakapi"))))
    (supported-systems '("x86_64-linux"))
    (native-inputs (list unzip))
    (home-page "https://wakapi.dev/")
    (synopsis "WakaTime-compatible backend")
    (description
     "This package provides @code{wakapi}, a WakaTime-compatible backend for
coding statistics.")
    (license license:expat)))

(define-public wakatime-cli-bin
  (package
    (name "wakatime-cli-bin")
    (version "1.75.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/wakatime/wakatime-cli"
                                  "/releases/download/v" version
                                  "/wakatime-cli-linux-amd64.zip"))
              (sha256
               (base32
                "10xpvfvmvfjsghav4128v6ligqszxy1xlvizjm0dnrynd0gq2i1m"))))
    (build-system copy-build-system)
    (arguments
     (list #:install-plan
           #~'(("wakatime-cli-linux-amd64" "bin/wakatime-cli"))))
    (supported-systems '("x86_64-linux"))
    (native-inputs (list unzip))
    (home-page "https://wakatime.com/plugins")
    (synopsis "Command line interface to WakaTime")
    (description
     "This package provides @code{wakatime-cli}, the command line interface to
WakaTime, which is used by all WakaTime text editor plugins.")
    (license license:bsd-3)))
