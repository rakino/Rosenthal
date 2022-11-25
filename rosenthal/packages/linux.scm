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

(define %xanmod-version "6.0.9")
(define %xanmod-revision "xanmod1")

(define (extract-xanmod-patch version hash)
  (let ((patch (string-append "linux-" version ".patch"))
        (source (origin
                  (method url-fetch)
                  (uri (string-append "https://github.com/xanmod/linux"
                                      "/releases/download/" version
                                      "/patch-" version ".xz"))
                  (sha256 (base32 hash)))))
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
   "0ar9k5bj75s4ac2a1xlm6l5x96jd6v2azpab19cx19hmgg3c3wh2"))

(define linux-xanmod-source
  (origin
    (inherit (%upstream-linux-source
              "6.0"
              (base32 "13kqh7yhifwz5dmd3ky0b3mzbh9r0nmjfp5mxy42drcdafjl692w")))
    (patches (list linux-xanmod-patch))))

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
