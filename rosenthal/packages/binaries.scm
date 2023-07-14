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
