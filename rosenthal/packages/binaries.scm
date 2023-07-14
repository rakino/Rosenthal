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
    (license (license "Application Terms of Service"
                      "https://www.cloudflare.com/application/terms/"
                      "nonfree"))))

(define-public hugo-bin
  (package
    (name "hugo-bin")
    (version "0.115.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/gohugoio/hugo" "/releases/download/v"
                    version "/hugo_extended_" version "_linux-amd64.tar.gz"))
              (sha256
               (base32
                "0i56yy77h1d4f36c2b9vlmdr2x6wzq9q3l7sck80wkz9awdjfzhv"))))
    (build-system copy-build-system)
    (arguments
     (list #:install-plan #~'(("hugo" "bin/hugo"))
           #:phases
           #~(modify-phases %standard-phases
               (delete 'strip)
               (add-after 'install 'patch-elf
                 (lambda _
                   (let ((hugo (string-append #$output "/bin/hugo")))
                     (invoke "patchelf" "--set-interpreter"
                             (string-append #$(this-package-input "glibc")
                                            #$(glibc-dynamic-linker))
                             hugo)
                     (invoke "patchelf" "--set-rpath"
                             (string-append #$gcc:lib "/lib")
                             hugo)))))))
    (supported-systems '("x86_64-linux"))
    (native-inputs (list patchelf))
    (inputs (list glibc))
    (home-page "https://gohugo.io/")
    (synopsis "Static site generator")
    (description
     "Hugo is a static site generator written in Go, optimized for speed and
designed for flexibility.")
    (license license:asl2.0)))

(define-public sing-box-bin
  (package
    (name "sing-box-bin")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/SagerNet/sing-box/releases/download/v"
                    version "/sing-box-" version "-linux-amd64v3.tar.gz"))
              (sha256
               (base32
                "1dn19l9zyj91fi8p1fjg76b7hhcg8l5lbjbig782zvjlfmbyjqyi"))))
    (build-system copy-build-system)
    (arguments (list #:install-plan #~'(("sing-box" "bin/sing-box"))))
    (supported-systems '("x86_64-linux"))
    (home-page "https://sing-box.sagernet.org/")
    (synopsis "Universal proxy platform")
    (description
     "This package provides @code{sing-box}, a universal proxy platform.")
    (license license:gpl3+)))
