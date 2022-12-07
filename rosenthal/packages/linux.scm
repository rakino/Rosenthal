;; SPDX-FileCopyrightText: 2022 Hilton Chain <hako@ultrarare.space>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (rosenthal packages linux)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (ice-9 match)
  #:use-module (guix build-system)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages tls))

(define computed-origin-method
  (@@ (guix packages) computed-origin-method))

(define deblob-scripts
  (@@ (gnu packages linux) deblob-scripts-6.0))

(define doc-supported?
  (@@ (gnu packages linux) doc-supported?))

(define make-linux-libre-source
  (@@ (gnu packages linux) make-linux-libre-source))

(define %upstream-linux-source
  (@@ (gnu packages linux) %upstream-linux-source))

(define linux-rosenthal-deblob-scripts
  (match deblob-scripts
    ((deblob-version (? origin? deblob) (? origin? deblob-check))
     (list deblob-version
           (origin
             (inherit deblob)
             (file-name "linux-libre-deblob")
             (patches (list (local-file "patches/linux-libre-deblob-keep-needed.patch"))))
           (origin
             (inherit deblob-check)
             (file-name "linux-libre-deblob-check")
             (patches (list (local-file "patches/linux-libre-deblob-check-omit-error.patch"))))))))

(define %cflags
  (string-append
   " -flto" " -fpic" " -fpie" " -fvisibility=hidden" " -fwrapv" " -pipe"
   " -fsanitize=cfi" " -fstack-clash-protection" " -fstack-protector-strong"
   " -enable-trivial-auto-var-init-zero-knowing-it-will-be-removed-from-clang"
   " -ftrivial-auto-var-init=zero -D_FORTIFY_SOURCE=2 -D_GLIBCXX_ASSERTIONS"))

(define %ldflags "-Wl,-z,defs -Wl,-z,now -Wl,-z,relro -Wl,-pie")

(define %xanmod-version "6.0.10")
(define %xanmod-revision "xanmod1")
(define %hardened-revision "hardened1")

(define (extract-xanmod-patch version hash)
  (let ((patch (string-append "linux-" version ".patch"))
        (source (origin
                  (method url-fetch)
                  (uri (string-append "https://github.com/xanmod/linux"
                                      "/releases/download/" version
                                      "/patch-" version ".xz"))
                  (sha256 hash))))
    (origin
      (method computed-origin-method)
      (file-name patch)
      (sha256 #f)
      (uri
       (delay
         (with-imported-modules '((guix build utils))
           #~(begin (use-modules (guix build utils))
                    (set-path-environment-variable
                     "PATH" '("bin") (list #+xz))
                    (setenv "XZ_OPT" (string-join (%xz-parallel-args)))
                    (map (lambda (p)
                           (begin
                             (copy-file #+source p)
                             (make-file-writable p)
                             (invoke "xz" "--decompress" p)))
                         (list (string-append #$patch ".xz")))
                    (copy-file #$patch #$output))))))))

(define linux-xanmod-patch
  (extract-xanmod-patch
   (string-append %xanmod-version "-" %xanmod-revision)
   (base32 "0ypvr7lp9bhlja3zp97vmfxa80144z1kplsrzqdj301xwrmiki37")))

(define linux-hardened-patch-for-xanmod
  (origin
    (method url-fetch)
    (uri (string-append "https://github.com/anthraxx/linux-hardened/releases/download/"
                        %xanmod-version "-" %hardened-revision "/linux-hardened-"
                        %xanmod-version "-" %hardened-revision ".patch"))
    (patches (list (local-file "patches/linux-hardened-xanmod-adaption.patch")))
    (sha256 (base32 "1zbhqwhbzjc2jsmbrqk6y4w62b9drhzh2kb1p5bwgi3nd17f43jj"))))

(define linux-xanmod-source
  (origin
    (inherit (%upstream-linux-source
              "6.0"
              (base32 "13kqh7yhifwz5dmd3ky0b3mzbh9r0nmjfp5mxy42drcdafjl692w")))
    (patches
     (append (list linux-xanmod-patch
                   linux-hardened-patch-for-xanmod)
             (if (doc-supported? %xanmod-version)
                 (search-patches "linux-libre-infodocs-target.patch")
                 '())))))

(define linux-rosenthal-source
  (make-linux-libre-source
   %xanmod-version
   linux-xanmod-source
   linux-rosenthal-deblob-scripts))

(define-public linux-xanmod
  (let ((base linux-libre))
    (package
      (inherit base)
      (name "linux-xanmod")
      (version %xanmod-version)
      (source linux-xanmod-source)
      (build-system
        (build-system-with-c-toolchain
         (package-build-system base)
         (modify-inputs (standard-packages)
           (delete "binutils" "gcc" "ld-wrapper"))))
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases)
          #~(modify-phases #$phases
              (add-before 'configure 'setenv
                (lambda _
                  (setenv "LLVM" "1")
                  (setenv "CFLAGS" #$%cflags)
                  (setenv "CXXFLAGS" #$%cflags)
                  (setenv "LDFLAGS" #$%ldflags)

                  ;; FIXME: For some unknown reason, the build would fail when
                  ;; calling `pthread_cancel()` at SORTTAB stage, together
                  ;; with a "missing libgcc_s.so.1" error, no matter whether
                  ;; it's present in LIBRARY_PATH.

                  ;; However, LD_PRELOAD addresses this....
                  (setenv "LD_PRELOAD"
                          (string-append #$gcc:lib "/lib/libgcc_s.so.1"))))

              ;; NOTE: As defined in `(make-linux-libre)`, `linux-libre` would
              ;; apply a few kernel configuration here, to workaround this
              ;; without defining `(make-linux-libre)` from scratch again,
              ;; simply replace `.config` with ours.
              (add-after 'configure 'replace-kconfig
                (lambda* (#:key inputs #:allow-other-keys)
                  (let ((config (assoc-ref inputs "kconfig")))
                    (copy-file config ".config")
                    (chmod ".config" #o666))))))))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (append clang-15
                 llvm-15
                 lld-as-ld-wrapper-15
                 python-minimal-wrapper
                 zstd)
         (delete "gmp" "mpc" "mpfr")))
      (home-page "https://xanmod.org/")
      (synopsis
       "Linux kernel distribution with custom settings and new features")
      (description
       "General-purpose Linux kernel distribution with custom settings and new
features.  Built to provide a stable, responsive and smooth desktop
experience."))))

(define-public linux-rosenthal
  (let ((base linux-xanmod))
    (package
      (inherit base)
      (name "linux-rosenthal")
      (source linux-rosenthal-source)
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (replace "kconfig"
           (local-file "aux-files/config.zen3-dorphine"))))
      (home-page "https://github.com/rakino/rosenthal/")
      (synopsis "Custom Linux kernel")
      (description
       "Linux-Rosenthal is a custom Linux kernel based on @code{linux-xanmod}.
This kernel is partially deblobed, with some files necessary to drive specific
hardwares kept."))))

(define-public kconfig-hardened-check-dev
  (let* ((base kconfig-hardened-check)
         (revision "135")
         (commit "d361925ba8e7c1f712615e12d4eff678f1f4d59b"))
    (package
      (inherit base)
      (name "kconfig-hardened-check-dev")
      (version (git-version "0.5.17" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/a13xp0p0v/kconfig-hardened-check")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0lkm4q3ndk5v58299sn4iclz0xf0z1hl63lkkmm05kgib0i4gbqk")))))))
